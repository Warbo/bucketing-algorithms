{ fail, ghcWithML4HSFE, haskellPackages, jq, makeSamples, mkBin, runCommand, tebenchmark,
  withDeps, wrap, writeScript }:

with rec {
  haskellVersion = runCommand "get-ground-truths-haskell"
    (tebenchmark.cache // {
      buildInputs = [ (ghcWithML4HSFE {}) ];
      helper      = writeScript "get-ground-truths-helper.hs" ''
        {-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
        module Helper where

        import           Control.Monad              (mzero)
        import qualified Data.Aeson                 as A
        import qualified Data.AttoLisp              as L
        import qualified Data.Attoparsec.ByteString as AP
        import qualified Data.ByteString.Char8      as B
        import qualified Data.ByteString.Lazy       as LB
        import qualified Data.Char                  as C
        import qualified Data.HashMap.Strict        as H
        import qualified Data.List                  as List
        import qualified Data.Maybe                 as M
        import qualified Data.Text                  as T
        import           Language.Haskell.TH.Syntax (Exp(..), Lift, lift)
        import qualified Numeric                    as N
        import qualified System.Environment         as Env
        import           System.IO.Unsafe           (unsafePerformIO)

        type TheoremID = String

        newtype Name = N T.Text deriving (Eq, Show)

        instance Lift Name where
          lift (N n) = do arg <- lift (T.unpack n)
                          pure (AppE (ConE 'N) (AppE (VarE 'T.pack) arg))

        instance A.FromJSON Name where
          parseJSON s = N <$> A.parseJSON s

        instance A.ToJSON Name where
          toJSON (N n) = A.toJSON n

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

        data Result = R {
            names    :: Either [Name] [[Name]]
          , theorems :: [TheoremID]
          }

        instance A.ToJSON Result where
          toJSON r = A.object [
              "names"    A..= either A.toJSON A.toJSON (names r)
            , "theorems" A..= A.toJSON (theorems r)
            ]
      '';

      main = writeScript "get-ground-truths-main.hs" ''
        {-# LANGUAGE BangPatterns, OverloadedStrings, TemplateHaskell #-}
        module Main where

        import           Helper
        import           Control.Monad              (mzero)
        import qualified BucketUtil                 as BU
        import qualified Data.Aeson                 as A
        import qualified Data.AttoLisp              as L
        import qualified Data.Attoparsec.ByteString as AP
        import qualified Data.ByteString.Char8      as B
        import qualified Data.ByteString.Lazy       as LB
        import qualified Data.Char                  as C
        import qualified Data.HashMap.Strict        as H
        import qualified Data.List                  as List
        import qualified Data.Maybe                 as M
        import qualified Data.Text                  as T
        import           Language.Haskell.TH.Syntax (lift, runIO)
        import qualified Numeric                    as N
        import qualified System.Environment         as Env
        import           System.IO                  (hPutStr, stderr)
        import           System.IO.Unsafe           (unsafePerformIO)

        theoremDeps :: [(TheoremID, [Name])]
        theoremDeps =
          $(let name = "BENCHMARKS_THEOREM_DEPS"

                lispToData l = case L.parseEither L.parseLisp l of
                                 Left  e -> error e
                                 Right d -> d

                strToData :: B.ByteString -> [(TheoremID, [Name])]
                strToData  s = case AP.parseOnly (L.lisp <* AP.endOfInput) s of
                                 Left  e -> error e
                                 Right l -> lispToData l

             in do mp <- runIO (Env.lookupEnv name)
                   s  <- case mp of
                           Nothing -> error ("Env doesn't contain " ++ name)
                           Just p  -> runIO (B.readFile p)
                   lift (strToData s))

        theoremFilesAdmittedBy :: [Name] -> [TheoremID]
        theoremFilesAdmittedBy sample = map fst (filter match theoremDeps)
          where match td = snd td `subset` sample

        mkResult :: Either [Name] [[Name]] -> Result
        mkResult ns = R { names = ns, theorems = either process nested ns }
          where process :: [Name] -> [TheoremID]
                process [] = []
                process ns = theoremFilesAdmittedBy (map decodeName ns)

                nested :: [[Name]] -> [TheoremID]
                nested []  = []
                nested nns = nub (concatMap process nns)

        addTruths :: LB.ByteString -> Result
        addTruths x = mkResult ns
          where ns = case A.decode x of
                       Just y  -> Left y
                       Nothing -> case A.decode x of
                                    Just ys -> Right ys
                                    Nothing -> err
                err = error ("Couldn't decode sample from: " ++ show x)

        augmentSize :: IO ()
        augmentSize = BU.streamKeyVals go
          where go key = do LB.putStr key
                            LB.putStr " : "
                            augmentRep

        -- We can't use streamKeyVals here, since reps may be null
        augmentRep :: IO ()
        augmentRep = do c <- BU.skipSpace
                        case c of
                          '{' -> putChar '{' >> go True
                          'n' -> do 'u' <- getChar
                                    'l' <- getChar
                                    'l' <- getChar
                                    LB.putStr "null"
                          _   -> error (show (("Wanted", "'{' or 'null'"),
                                              ("Got",    c)))
          where go first = do mk <- BU.parseOne
                              case mk of
                                Nothing -> putChar '}' -- We've hit, and consumed, the '}'
                                Just !k -> do if first
                                                 then pure ()
                                                 else putChar ','
                                              f (BU.trimKey k)
                                              go False
                f key = do LB.putStr key
                           LB.putStr " : "
                           if key == "\"sample\""
                              then do findColon
                                      Just str <- BU.parseOne
                                      LB.putStr (A.encode (addTruths str))
                              else BU.streamKeyVals augmentRun

        findColon = do c <- getChar
                       case c of
                         ':' -> pure ()
                         _ | C.isSpace c -> findColon
                         _ -> error ("Looking for ':' found " ++ show c)

        augmentRun :: LB.ByteString -> IO ()
        augmentRun key = do LB.putStr key
                            LB.putStr " : "
                            findColon
                            Just str <- BU.parseOne
                            LB.putStr (A.encode (addTruths str))

        main = BU.streamKeyVals go
          where go key = do LB.putStr key
                            LB.putStr " : "
                            augmentSize
      '';
    })
    ''
      cp "$helper" Helper.hs
      cp "$main"   Main.hs
      cp "${../haskell-support/BucketUtil.hs}" BucketUtil.hs
      ghc --make -o Main Main.hs
      mv Main "$out"
    '';

  go = wrap {
    name = "get-ground-truths";
    file = haskellVersion;
  };

  test = runCommand "test-get-ground-truths"
    {
      inherit go;
      buildInputs = [ fail jq ];
      samples     = makeSamples { sizes = [ 1 5 10 ]; reps = 3; };
      smallSample = makeSamples { sizes = [ 2 ]; reps = 1; };
    }
    ''
      echo -e "\nTesting non-JSON" 1>&2
      O=$(echo 'nope' | "$go" 2>&1) && fail "Non-JSON should error\n$O"
      echo "Non-JSON passed" 1>&2

      echo -e "\nTesting empty JSON object" 1>&2
      O=$(echo '{}'   | "$go") || fail "JSON object should work\n$O"
      [[ "x$O" = "x{}" ]]      || fail "Object should give '{}', got '$O'"
      echo "Empty JSON object passed" 1>&2

      echo -e "\nTesting JSON array" 1>&2
      O=$(echo '[]'   | "$go" 2>&1) && fail "JSON array should fail\n$O"
      echo "JSON array passed" 1>&2

      echo -e "\nTesting JSON null" 1>&2
      O=$(echo 'null' | "$go" 2>&1) && fail "JSON null should fail\n$O"
      echo "JSON null passed" 1>&2

      echo -e "\nTesting non-empty object" 1>&2
      O=$(echo '{"1":{"2":{"sample":[]}}}' | "$go") ||
        fail "Non-empty object failed\n$O"
      WANT='{"1":{"2":[{"sampleNames":[],"sampleTheorems":[]}]}}'
      echo "$O" | jq -e --argjson want "$WANT" '. == $want' ||
        fail "Non-empty object should produce '$WANT', got '$O'"
      unset WANT

      echo -e "\nTesting non-empty sample" 1>&2
      O=$(echo '{"1":{"2":{"sample":["global64"]}}}' | "$go") ||
        fail "Non-empty sample failed"
      echo "$O" | jq -e '.["1"] | .["2"] | .[0] | type | . == "object"' ||
        fail "Non-empty sample should become an object, got '$O'"
      echo "$O" | jq -e '.["1"] | .["2"] | .[0] | has("sampleNames")' ||
        fail "Non-empty sample should get 'sampleNames', got '$O'"
      echo "$O" | jq -e '.["1"] | .["2"] | .[0] | has("sampleTheorems")' ||
        fail "Non-empty sample should have 'sampleTheorems', got '$O'"
      echo "$O" | jq -e '.["1"] | .["2"] | .[0] | .sampleNames |
                                  . == ["global64"]' ||
        fail "'sampleNames' should match input '[\"global64\"]', got '$O'"
      echo "$O" | jq -e '.["1"] | .["2"] | .[0] | .sampleTheorems | . == []' ||
        fail "Bogus names should get empty 'sampleTheorems', got '$O'"
      echo "Non-empty sample passed" 1>&2

      echo -e "\nTesting small sample" 1>&2
      O=$("$go" < "$smallSample") || fail "Didn't get small ground truths"
      echo "$O" | jq -e 'map(map(.[0] | .sampleTheorems
                                      | length
                                      | . > 0) | all) | all' ||
        fail "Small samples should have at least one theorem '$O'"
      echo "Small sample passed" 1>&2

      echo -e "\nTesting larger sample" 1>&2
      O=$("$go" < "$samples") || fail "Didn't get ground truths of samples"
      echo "$O" | jq -e 'map(map(.[0] | .sampleTheorems
                                      | length
                                      | . > 0) | all) | all' ||
        fail "Every sample should have at least one theorem '$O'"
      echo "Larger sample passed" 1>&2

      mkdir "$out"
    '';
};
withDeps [ test ] go
