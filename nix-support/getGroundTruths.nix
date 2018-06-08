{ fail, haskellPackages, jq, makeSamples, mkBin, runCommand, tebenchmark,
  withDeps, wrap, writeScript }:

with rec {
  haskellVersion = runCommand "get-ground-truths-haskell"
    {
      buildInputs = [ (haskellPackages.ghcWithPackages (h: [
        h.aeson h.atto-lisp h.attoparsec h.bytestring h.text
      ])) ];
      main = writeScript "get-ground-truths-main.hs" ''
        {-# LANGUAGE OverloadedStrings #-}
        module Main where

        import           Control.Monad              (mzero)
        import qualified Data.Aeson                 as A
        import qualified Data.AttoLisp              as L
        import qualified Data.Attoparsec.ByteString as AP
        import qualified Data.ByteString.Char8      as B
        import qualified Data.ByteString.Lazy       as LB
        import qualified Data.Char                  as C
        import qualified Data.Maybe                 as M
        import qualified Data.Text                  as T
        import qualified Numeric                    as N
        import qualified System.Environment         as Env
        import           System.IO.Unsafe           (unsafePerformIO)

        type TheoremID = String

        newtype Name = N T.Text deriving (Eq, Show)

        instance A.FromJSON Name where
          parseJSON s = N <$> A.parseJSON s

        -- Our cached data uses symbols, which may be escaped if they
        -- contain e.g. "'". Escaped symbols are wrapped in pipes "|".
        unescapeSym s = case T.stripPrefix "|" s of
                          Nothing -> s
                          Just s2 -> case T.stripSuffix "|" s2 of
                            Nothing  -> s
                            Just s3 -> s3

        instance L.FromLisp Name where
          parseLisp (L.Symbol n) = pure (N (unescapeSym n))
          parseLisp _            = mzero

        newtype TheoremDeps = TDs [(TheoremID, [Name])]

        instance L.FromLisp TheoremDeps where
          parseLisp l = do tds <- L.parseLisp l
                           pure (TDs (map toPair tds))
            where toPair [Left id, Right deps] = (id, deps)

        theoremFilesAdmittedBy :: [Name] -> [TheoremID]
        theoremFilesAdmittedBy sample = map fst (filter match theoremDeps)
          where match td = snd td `subset` sample

        theoremDeps :: [(TheoremID, [Name])]
        theoremDeps = unsafePerformIO go
          where go = do
                  mp <- Env.lookupEnv name
                  case mp of
                    Nothing -> error ("Environment doesn't contain " ++ name)
                    Just p  -> strToData <$> B.readFile p

                name = "BENCHMARKS_THEOREM_DEPS"

                lispToData l = case L.parseEither L.parseLisp l of
                                 Left  e -> error e
                                 Right d -> d

                strToData  s = case AP.parseOnly (L.lisp <* AP.endOfInput) s of
                                 Left  e -> error e
                                 Right l -> lispToData l

        subset []     ys = True
        subset (x:xs) ys = (x `elem` ys) && (xs `subset` ys)

        decodeName (N n) = case M.catMaybes [T.stripPrefix "global" n,
                                             T.stripPrefix "Global" n] of
                             []   -> error ("Bad name " ++ show n)
                             n2:_ -> N (T.pack (decodeASCII "" (T.unpack n2)))
          where decodeASCII acc s = case s of
                                      []     -> reverse acc
                                      a:b:cs -> decodeASCII (unHex a b:acc) cs
                unHex a b = case N.readHex [a, b] of
                              [(n, "")] -> C.chr n
                              _         -> error (show ("Invalid hex", a, b))

        main = LB.interact (A.encode . theoremFilesAdmittedBy . parse)
          where parse s = case A.eitherDecode s of
                            Left  e -> error e
                            Right x -> map decodeName x
      '';
    }
    ''
      cp "$main" Main.hs
      ghc --make -o Main Main.hs
      mv Main "$out"
    '';

  go = wrap {
    name   = "get-ground-truths.rkt";
    paths  = [
      tebenchmark.env
      (mkBin { name = "haskellVersion"; file = haskellVersion; })
    ];
    vars   = tebenchmark.cache // { inherit haskellVersion; };
    script = ''
      #!/usr/bin/env racket
      #lang racket
      (require        json)
      (require        rackunit)
      (require        shell/pipeline)
      (require        lib/conjectures)
      (require/expose lib/conjectures (theorem-files-admitted-by))
      (require        lib/normalise)
      (require        lib/util)

      (define (err x)
        (error (format "~S" x)))

      (define (theorems-of encoded-names)
        (string->jsexpr
          (run-pipeline/out `(echo ,(jsexpr->string encoded-names))
                            '(haskellVersion))))

      ;; Ground truth for a sample (list of encoded names)
      (define (theorems-of-sample sample)
        (eprintf "theorems-of-sample\n")
        (unless (list? sample)
          (err `((error  "Expected sample to be a list")
                 (sample ,sample))))
        (theorems-of sample))

      ;; Ground truth for a list of samples (e.g. buckets)
      (define (theorems-of-sample-list samples)
        (eprintf "theorems-of-sample-list\n")
        (remove-duplicates
          (foldl (lambda (sample theorems)
                   (eprintf "\n~S\n" `((theorems ,theorems) (sample ,sample)))
                   (append theorems (theorems-of-sample sample)))
                 '()
                samples)))

      ;; Takes a sample or list of samples, returns it with ground truth
      (define (add-ground-truth sample-or-list)
        (eprintf "add-ground-truth\n")
        (make-immutable-hash
          `((names    . ,sample-or-list)
            (theorems . ,(cond
                           [(not (list? sample-or-list))
                            (err `((error "Should have got a list")
                                   (sample-or-list ,sample-or-list)))]

                           [(empty? sample-or-list) '()]

                           [(string? (first sample-or-list))
                            (theorems-of-sample      sample-or-list)]

                           [(list?   (first sample-or-list))
                            (theorems-of-sample-list sample-or-list)]

                           [#t (err
                             `((error "Unexpected list type")
                               (sample-or-list ,sample-or-list)))])))))

      ;; Adds ground truths to any lists in the given map, recursively
      (define (add-ground-truths data)
        (eprintf "add-ground-truths\n")
        (hash-foldl (lambda (key value result)
                      (hash-set result key
                        (cond
                          [(list?  value) (add-ground-truth  value)]
                          [(hash?  value) (add-ground-truths value)]
                          [(equal? value 'null) value]

                          [#t (err `((error  "Unexpected type")
                                     (key    ,key)
                                     (value  ,value)
                                     (result ,result)))])))
                    (make-immutable-hash '())
                    data))

      ;; Add ground truths to stdio
      (write-json (add-ground-truths (string->jsexpr (port->string))))
    '';
  };

  test = runCommand "test-get-ground-truths"
    {
      inherit go;
      buildInputs = [ fail jq ];
      samples     = makeSamples { sizes = [ 1 5 10 ]; reps = 3; };
      smallSample = makeSamples { sizes = [ 2 ]; reps = 1; };
    }
    ''
      O=$(echo 'nope' | "$go" 2>&1) && fail "Non-JSON should error\n$O"

      O=$(echo '{}'   | "$go") || fail "JSON object should work\n$O"
      [[ "x$O" = "x{}" ]]      || fail "Object should give '{}', got '$O'"

      O=$(echo '[]'   | "$go" 2>&1) && fail "JSON array should fail\n$O"
      O=$(echo 'null' | "$go" 2>&1) && fail "JSON null should fail\n$O"

      O=$(echo '{"x":[]}' | "$go") || fail "Non-empty object failed\n$O"
      WANT='{"x":{"names":[],"theorems":[]}}'
      echo "$O" | jq -e --argjson want "$WANT" '. == $want' ||
        fail "Non-empty object should produce '$WANT', got '$O'"
      unset WANT

      O=$(echo '{"x":["global64"]}' | "$go") || fail "Non-empty array failed"
      echo "$O" | jq -e '.x | type | . == "object"' ||
        fail "Non-empty array's 'x' should be object, got '$O'"
      echo "$O" | jq -e '.x | has("names")' ||
        fail "Non-empty array's 'x' should have 'names', got '$O'"
      echo "$O" | jq -e '.x | has("theorems")' ||
        fail "Non-empty array's 'x' should have 'theorems', got '$O'"
      echo "$O" | jq -e '.x | .names | . == ["global64"]' ||
        fail "Non-empty 'names' should match input '[\"global64\"]', got '$O'"
      echo "$O" | jq -e '.x | .theorems | . == []' ||
        fail "Bogus names should get empty 'theorems', got '$O'"

      O=$("$go" < "$smallSample") || fail "Didn't get small ground truths"
      echo "$O" | jq -e 'map(map(.sample | .theorems
                                         | length
                                         | . > 0) | all) | all' ||
      fail "Small samples should have at least one theorem '$O'"

      O=$("$go" < "$samples") || fail "Didn't get ground truths of samples"
      echo "$O" | jq -e 'map(map(.sample | .theorems
                                         | length
                                         | . > 0) | all) | all' ||
        fail "Every sample should have at least one theorem '$O'"

      mkdir "$out"
    '';
};
withDeps [ test ] go
