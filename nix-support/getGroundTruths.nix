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
        import qualified Data.List                  as List
        import qualified Data.Maybe                 as M
        import qualified Data.Text                  as T
        import qualified Numeric                    as N
        import qualified System.Environment         as Env
        import           System.IO.Unsafe           (unsafePerformIO)

        type TheoremID = String

        newtype Name = N T.Text deriving (Eq, Show)

        instance A.FromJSON Name where
          parseJSON s = N <$> A.parseJSON s


        instance L.FromLisp Name where
          parseLisp l = case l of
                          L.Symbol n -> pure (N (unescapeSym n))
                          _          -> mzero
            -- Our cached data uses symbols, which may be escaped if they
            -- contain e.g. "'". Escaped symbols are wrapped in pipes "|".
            where unescapeSym s = case T.stripPrefix "|" s of
                                    Nothing -> s
                                    Just s2 -> case T.stripSuffix "|" s2 of
                                                 Nothing  -> s
                                                 Just s3 -> s3

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

        nub :: (Eq a, Ord a) => [a] -> [a]
        nub = go [] . List.sort
          where go acc l  = case l of
                              []       ->   acc
                              [x]      -> x:acc
                              (x:y:zs) -> if x == y
                                             then go    acc  (y:zs)
                                             else go (x:acc) (y:zs)

        main = LB.interact (A.encode . go)
          where go s = case A.eitherDecode s of
                         Right x -> process x
                         Left e1 -> case A.eitherDecode s of
                                      Left  e2 -> err e1 e2
                                      Right xs -> nested xs

                err e1 e2 = error ("No parse: " ++ e1 ++ "\n\n" ++ e2)

                process :: [Name] -> [TheoremID]
                process [] = []
                process ns = theoremFilesAdmittedBy (map decodeName ns)

                nested :: [[Name]] -> [TheoremID]
                nested []  = []
                nested nns = nub (concatMap process nns)
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
      (require json)
      (require shell/pipeline)
      (require lib/conjectures)
      (require lib/normalise)
      (require lib/util)

      (define (err x)
        (error (format "~S" x)))

      ;; Ground truth for a sample (list of encoded names)
      (define (theorems-of-sample sample-or-list)
        (string->jsexpr
          (run-pipeline/out `(echo ,(jsexpr->string sample-or-list))
                            '(haskellVersion))))

      ;; Takes a sample or list of samples, returns it with ground truth
      (define (add-ground-truth sample-or-list)
        (eprintf "add-ground-truth\n")
        (make-immutable-hash
          `((names    . ,sample-or-list)
            (theorems . ,(theorems-of-sample sample-or-list)))))

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
