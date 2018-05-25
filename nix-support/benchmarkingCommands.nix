# Commands useful for generating and bucketing test data. These have TEBenchmark
# baked in, so we only need to provide a sample of names and their corresponding
# ASTs will be looked up internally.
#
# We benchmark these in two ways:
#
#  - The impact of the bucketing algorithms is measured, on lots of test data
#  - The speed of the scripts is measured on small inputs, to aid us in
#    optimising their implementation (since the above can be very slow!)
{ bash, buckets, fail, haskellPackages, jq, lib, nixpkgs, runCommand,
  tebenchmark, testData, withDeps, wrap, writeScript }:

with builtins;
rec {
  astsOf =
    with rec {
      script = runCommand "astsOf"
        {
          buildInputs = [
            (haskellPackages.ghcWithPackages (h: [
              h.aeson
            ]))
          ];
          main = writeScript "Main.hs" ''
            {-# LANGUAGE OverloadedStrings #-}
            import Control.Monad (mzero)
            import qualified Data.Aeson                 as Aeson
            import qualified Data.ByteString.Lazy.Char8 as BS
            import qualified Data.Map.Lazy              as Map
            import qualified Data.Text                  as T
            import qualified System.IO.Unsafe

            -- Data types and JSON parsers/printers

            newtype Name = Name { unName :: T.Text } deriving (Eq, Ord)

            instance Aeson.FromJSON Name where
              parseJSON x = Name <$> Aeson.parseJSON x

            newtype AST = AST  Aeson.Object

            instance Aeson.ToJSON AST where
              toJSON (AST x) = Aeson.toJSON x

            newtype NamedAST = NAST { unNamed :: (Name, AST) }

            type ASTMap = Map.Map Name AST

            instance Aeson.FromJSON NamedAST where
              parseJSON (Aeson.Object o) = do n <- o Aeson..: "name"
                                              pure (NAST (n, AST o))
              parseJSON _                = mzero

            -- Maps names to their ASTs/metadata, reading from a known file
            astMap :: ASTMap
            astMap = Map.fromList (map unNamed asts)
              where asts :: [NamedAST]
                    asts = case Aeson.eitherDecode encoded of
                                Left err -> error err
                                Right xs -> xs
                    encoded = System.IO.Unsafe.unsafePerformIO
                                (BS.readFile "${testData.tip-benchmark.asts}")

            -- Parse, lookup, print

            astsOf = map get
              where get n        = Map.findWithDefault (err n) n astMap
                    err (Name n) = error ("No AST for " ++ show n)

            -- Reads JSON array of names from stdin, prints JSON array of ASTs
            main = BS.interact namesToAsts
              where namesToAsts :: BS.ByteString -> BS.ByteString
                    namesToAsts s = case Aeson.eitherDecode s of
                                         Left err -> error err
                                         Right ns -> Aeson.encode
                                                       (astsOf ns)
          '';
        }
        ''
          cp "$main" Main.hs
          ghc --make Main.hs -o "$out"
        '';

      test = runCommand "test-astsOf"
        {
          inherit script;
          buildInputs = [ fail jq tebenchmark.tools ];
        }
        ''
          S=$(choose_sample 5 10)                    || fail "Didn't sample"
          I=$(echo "$S" | jq -R '.' | jq -s '.')     || fail "Didn't wrap"
          O=$(echo "$I" | "$script")                 || fail "Didn't get ASTs"
          echo "$O" | jq -e 'type | . == "array"'    || fail "$O\nNot object"
          echo "$O" | jq -e 'length | . == 5'        || fail "$O\nNot 5 ASTs"
          echo "$O" | jq --argjson i "$I" \
                         'map(.name) | sort | . == ($i | sort)' ||
            fail "Input:\n$I\n\nOutput:\n$O\n\nMismatching names"
          mkdir "$out"
        '';
    };
    withDeps [ test ] script;

  # Run the bucket script on each sample; we use a few bucket sizes, in
  # increments
  addHashBucketsCmd = wrap {
    name  = "hash";
    paths = [ buckets.hashes jq ];
    vars  = {
      inherit astsOf;
      sizes = concatStringsSep " " (map toString (lib.range 1 20));
    };
    script = ''
      #!/usr/bin/env bash
      set -e
      ASTS=$("$astsOf")

      for CLUSTER_SIZE in $sizes
      do
        export CLUSTER_SIZE
        echo "$ASTS" | hashBucket |
          jq '{(env["CLUSTER_SIZE"]) : map(map(.name))}'
      done | jq -s 'add'
    '';
  };

  makeDupeSamples =
    with rec {
      script = wrap {
        name   = "makeDupeSamples.rkt";
        paths  = [ tebenchmark.env ];
        vars   = tebenchmark.cache;
        script = ''
          #!/usr/bin/env racket
          #lang racket
          (require json)
          (require lib/sampling)

          (define sizes
            (if (getenv "sizes")
                (string->jsexpr (getenv "sizes"))
                (if (getenv "maxSize")
                    (range 1 (+ 1 (string->number (getenv "maxSize"))))
                    (error "No sizes or maxSize env var given"))))

          (define reps
            (range 0 (string->number (getenv "reps"))))

          (write-json
            (make-immutable-hash
              (map (lambda (size)
                     (cons (string->symbol (~a size))
                           (make-immutable-hash
                             (map (lambda (rep)
                                    (eprintf
                                      (format "Size ~a rep ~a\n" size rep))
                                    (cons (string->symbol (~a rep))
                                          (map ~a
                                            (set->list
                                              (sample-from-benchmarks size
                                                                      rep)))))
                                    reps))))
                   sizes)))
        '';
      };

      go = env: runCommand "test-makeDupeSamples-${env.tag}"
                           (env // {
                             inherit script;
                             reps        = "2";
                             buildInputs = [ fail jq ];
                           });

      testBroke = go { tag = "broke"; } ''
        if "$script"
        then
          fail "Shouldn't have succeeded without maxSize or sizes"
        fi
        mkdir "$out"
      '';

      testMax = go { tag = "max"; maxSize = "3"; } ''
        "$script" > result

        function go {
          if jq -e "$1" < result
          then
            echo "PASS: $2" 1>&2
          else
            cat result 1>&2
            fail "FAIL: $2"
          fi
        }

        go 'type | . == "object"' "Sizes are object"
        go 'keys | sort | . == ["1", "2", "3"]' "Sizes are 1,2,3"
        go 'map_values(type | . == "object") | all' "Reps are objects"
        go 'map_values(keys | sort | . == ["0", "1"]) | all' "Reps are 0,1"
        for SIZE in 1 2 3
        do
          go ".[\"$SIZE\"] | map_values(length | . == $SIZE) | all" \
             "Sizes are $SIZE"
        done
        mkdir "$out"
      '';

      testSizes = go { tag = "sizes"; sizes = toJSON [ 1 5 14 2 ]; } ''
        "$script" > result

        function go {
          if jq -e "$1" < result
          then
            echo "PASS: $2" 1>&2
          else
            cat result 1>&2
            fail "FAIL: $2"
          fi
        }

        go 'type | . == "object"' "Sizes are object"
        go 'keys | sort | . == ["1", "14", "2", "5"]' "Sizes are 1,2,5,14"
        go 'map_values(type | . == "object") | all' "Reps are objects"
        go 'map_values(keys | sort | . == ["0", "1"]) | all' "Reps are 0,1"
        for SIZE in 1 2 5 14
        do
          go ".[\"$SIZE\"] | map_values(length | . == $SIZE) | all" \
             "Sizes are $SIZE"
        done
        mkdir "$out"
      '';
    };
    withDeps [ testBroke testMax testSizes ] script;

  dedupeSamples = wrap {
    name = "dedupe.py";
    paths = [ nixpkgs.python3 ];
    script = ''
      #!/usr/bin/env python3
      import json
      import sys

      data = json.loads(sys.stdin.read())
      for size in data:
        seen = []
        for iRep in sorted([int(rep) for rep in data[size]]):
          rep = str(iRep)
          sample = frozenset(data[size][rep])
          if sample in seen:
            data[size][rep] = None
          else:
            data[size][rep] = {'sample': data[size][rep]}
          seen += [sample]
      print(json.dumps(data))
    '';
  };
}
