# Command to read annotated ASTs from stdin, and write them to stdout grouped
# into "buckets". Chooses what to put in each bucket arbitrarily by using a
# hash of the names.
{ fail, haskellPackages, jq, mkBin, runCommand, withDeps, writeScript }:
with rec {
  hashBucket = runCommand "hashBucket"
    {
      buildInputs = [ (haskellPackages.ghcWithPackages (h: [
        h.aeson h.cryptonite h.memory h.unordered-containers
      ])) ];
      main = writeScript "hashBucket-main.hs" ''
        {-# LANGUAGE BangPatterns      #-}
        {-# LANGUAGE OverloadedStrings #-}
        import           Control.Applicative        ((<|>))
        import           Control.Monad              (mzero)
        import qualified Crypto.Hash                as H
        import qualified Data.Aeson                 as A
        import           Data.ByteArray             (convert)
        import qualified Data.ByteString            as BS
        import qualified Data.ByteString.Lazy.Char8 as BL
        import           Data.Char                  (isSpace)
        import qualified Data.HashMap.Strict        as HM
        import qualified Data.List                  as L
        import qualified Data.Map.Strict            as Map
        import           Data.Maybe                 (fromJust)
        import qualified Data.Text                  as T
        import qualified Data.Text.Encoding         as TE
        import           System.Environment         (lookupEnv)
        import           System.IO.Unsafe           (unsafePerformIO)

        data AST = AST {
          getName :: !T.Text,
          getAST  :: !A.Object,
          keeper  :: !Bool
        }

        instance A.ToJSON AST where
          toJSON = A.Object . getAST

        instance A.FromJSON AST where
          parseJSON (A.Object o) = do
            n <- o A..:  "name"
            t <- o A..:? "type"
            q <- o A..:? "quickspecable"
            pure (AST {
              getName = n,
              getAST  = HM.delete "features" o,
              keeper  = case (q, t :: Maybe String) of
                             (Just True, Just _) -> True
                             _                   -> False
            })
          parseJSON _ = mzero

        newtype Input = Input { unInput :: [AST] }

        instance A.FromJSON Input where
          parseJSON j = case j of
            A.Object _ -> do ast  <- A.parseJSON j
                             pure (Input [ast])
            A.Array  _ -> do asts <- A.parseJSON j
                             pure (Input asts)
            _          -> mzero

        clusters :: Int
        clusters = fromJust (fromSize <|> fromEnv <|> Just fromIn)
          where fromSize = case (unsafePerformIO (lookupEnv "CLUSTER_SIZE")) of
                             Nothing -> Nothing
                             Just s  -> let size = fromIntegral (read s :: Int)
                                            len  = fromIntegral inCount :: Float
                                         in Just (ceil (len / size))
                fromEnv = fmap read (unsafePerformIO (lookupEnv "CLUSTERS"))
                fromIn  = ceil (sqrt (fromIntegral inCount))
                inCount = length input
                ceil    = ceiling :: Float -> Int

        bucket :: [AST] -> [[AST]]
        bucket = go (Map.fromList [(i - 1, []) | i <- [1..clusters]])
          where go !acc []     = Map.elems acc
                go !acc (a:as) = let (c, a') = pickBucket a
                                 in go (addToBucket c a' acc) as

        type BucketMap = Map.Map Int [AST]

        addToBucket :: Int -> AST -> BucketMap -> BucketMap
        addToBucket i v !m = Map.alter insert i m
          where insert Nothing   = Just [v]
                insert (Just vs) = Just (v:vs)

        pickBucket :: AST -> (Int, AST)
        pickBucket x = (cluster, x { getAST = ast' })
          where cluster :: Num a => a
                cluster = fromInteger (num `mod` toInteger clusters)
                name    = getName x
                ast     = getAST  x
                ast'    = HM.insert "cluster" (A.Number (1 + cluster)) ast
                num     = bsToInteger hash
                hash    = convert (H.hashWith H.SHA256 (TE.encodeUtf8 name))

        bsToInteger :: BS.ByteString -> Integer
        bsToInteger = BS.foldl appendByte 0
          where appendByte !n b = (n * 256) + toInteger b

        input = unInput (parse (unsafePerformIO BL.getContents))
          where parse s = case A.eitherDecode s of
                  Left err -> if BL.all isSpace s
                                 then Input []
                                 else abort ["Failed to parse input", err]
                  Right x  -> x

        abort = error . L.intercalate " "

        render :: [[AST]] -> BL.ByteString
        render = go (go (A.encode . getAST) (Just keeper)) Nothing
          where go :: (a -> BL.ByteString)
                   -> Maybe (a -> Bool)
                   -> [a]
                   -> BL.ByteString
                go f p = BL.cons '['          .
                         (`BL.snoc` ']')      .
                         BL.intercalate ",\n" .
                         map f                .
                         maybe id filter p

        main = BL.putStrLn (render (bucket input))
      '';
    }
    ''
      cp "$main" Main.hs
      ghc --make Main.hs -o "$out"
    '';

  cmd = mkBin {
    name = "hashBucket";
    file = hashBucket;
  };

  hashCheck = runCommand "hash-bucket-check"
    { buildInputs = [ fail cmd jq ]; }
    ''
      set -e
      set -o pipefail

      echo "Testing empty input" 1>&2
      echo "" | CLUSTER_SIZE=10 hashBucket | jq -e 'length | . == 0'

      echo "Testing single input" 1>&2
      O='{"name":"foo", "type": "T", "quickspecable": true}'
      echo "[$O]" | CLUSTER_SIZE=10 hashBucket |
        jq -e --argjson o "$O" '. == [[$o + {"cluster":1}]]'

      O=$(echo '[{"name":"foo", "type":"T", "quickspecable":true},
                 {"name":"bar", "type":"U", "quickspecable":true}]' |
            CLUSTER_SIZE=1 hashBucket) || fail "Didn't bucket"
      echo "$O" | jq -e 'type | . == "array"' ||
        fail "Wrong result type"
      echo "$O" | jq -e 'length | . == 2' ||
        fail "Wrong number of buckets"
      echo "$O" | jq -e 'map(type | . == "array") | all' ||
        fail "Wrong bucket types"
      echo "$O" | jq -e 'map(length | . == 1) | all' ||
        fail "Wrong bucket lengths"
      echo "$O" | jq -e 'map(.[] | .name) | sort | . == ["bar", "foo"]' ||
        fail "Wrong names"

      mkdir "$out"
    '';
};
withDeps [ hashCheck ] cmd
