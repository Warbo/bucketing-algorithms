# Command to read annotated ASTs from stdin, and write them to stdout grouped
# into "buckets". Chooses what to put in each bucket arbitrarily by using a
# hash of the names.
{ bash, bc, fail, format, haskellPackages, jq, mkBin, runCommand, withDeps,
  writeScript }:
with rec {
  haskellVersion = runCommand "hashBucket"
    {
      buildInputs = [ (haskellPackages.ghcWithPackages (h: [
        h.aeson h.cryptonite h.memory h.unordered-containers
      ])) ];
      main = writeScript "hashBucket-main.hs" ''
        {-# LANGUAGE OverloadedStrings #-}
        import           Control.Monad              (mzero)
        import qualified Crypto.Hash                as H
        import qualified Data.Aeson                 as A
        import           Data.ByteArray             (convert)
        import qualified Data.ByteString            as BS
        import qualified Data.ByteString.Lazy.Char8 as BL
        import qualified Data.HashMap.Strict        as HM
        import qualified Data.Map.Strict            as Map
        import qualified Data.Text                  as T
        import qualified Data.Text.Encoding         as TE
        import qualified Debug.Trace                as Trace
        import           System.Environment         (lookupEnv)
        import           System.IO.Unsafe           (unsafePerformIO)

        data AST = AST {
          getName :: T.Text,
          getAST  :: A.Object,
          keeper  :: Bool
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

        clusters :: Int
        clusters = case ms of
            Nothing -> error "No clCount given"
            Just s  -> read s
          where ms = unsafePerformIO (lookupEnv "clCount")

        bucket :: [AST] -> [[AST]]
        bucket = go (Map.fromList [(i - 1, []) | i <- [1..clusters]])
          where go acc []     = Map.elems acc
                go acc (a:as) = let (c, a') = pickBucket a
                                 in go (addToBucket c a' acc) as

        type BucketMap = Map.Map Int [AST]

        addToBucket :: Int -> AST -> BucketMap -> BucketMap
        addToBucket i v m = Map.alter insert i m
          where insert Nothing   = Just [v]
                insert (Just vs) = Just (v:vs)

        pickBucket :: AST -> (Int, AST)
        pickBucket x = (cluster, x { getAST  = ast' })
          where cluster :: Num a => a
                cluster = fromInteger (num `mod` toInteger clusters)
                name    = getName x
                ast     = getAST  x
                ast'    = HM.insert "cluster" (A.Number (1 + cluster)) ast
                num     = bsToInteger hash
                hash    = convert (H.hashWith H.SHA256 (TE.encodeUtf8 name))

        bsToInteger :: BS.ByteString -> Integer
        bsToInteger = BS.foldl appendByte 0
          where appendByte n b = (n * 256) + toInteger b

        render :: [[AST]] -> BL.ByteString
        render = (\x -> Trace.trace ("Output is " ++ show x) x) .
                 go (go (A.encode . getAST) (Just keeper)) Nothing
          where go :: (a -> BL.ByteString)
                   -> Maybe (a -> Bool)
                   -> [a]
                   -> BL.ByteString
                go f p = BL.cons '['          .
                         (`BL.snoc` ']')      .
                         BL.intercalate ",\n" .
                         map f                .
                         maybe id filter p

        main = BL.interact (render . bucket . parse)
          where parse s = Trace.trace ("Input is:\n" ++ show s) $ case A.eitherDecode s of
                  Left err -> error err
                  Right x  -> x
      '';
    }
    ''
      cp "$main" Main.hs
      ghc --make Main.hs -o "$out"
    '';

  hashes = mkBin {
    name   = "hashBucket";
    paths  = [ bash bc haskellPackages.ghc jq ];
    vars   = { SIMPLE = "1"; };
    script = ''
      #!/usr/bin/env bash
      set -e
      set -o pipefail

      INPUT=$(cat)

      # Empty input turns into an empty array
      if [[ -z "$INPUT" ]]
      then
        INPUT='[]'
      fi

      # Wrap up raw objects into an array
      if echo "$INPUT" | jq -r 'type' | grep 'object' > /dev/null
      then
        INPUT=$(echo "$INPUT" | jq -s '.')
      fi

      if [[ -n "$CLUSTER_SIZE" ]]
      then
        echo "Using cluster size of $CLUSTER_SIZE" 1>&2
        LENGTH=$(echo "$INPUT" | jq 'length')
        [[ -n "$LENGTH" ]] || LENGTH=0

        PROG=$(echo "main = print (ceiling (($LENGTH :: Float) / $CLUSTER_SIZE) :: Int)")
        CLUSTERS=$(echo "$PROG" | runhaskell)

        echo "Using $CLUSTERS clusters of length $CLUSTER_SIZE" 1>&2
      fi

      [[ -n "$CLUSTERS" ]] || {
        CLUSTERS=$(echo "$INPUT" | jq 'length | sqrt | . + 0.5 | floor')
        export CLUSTERS

        echo "No cluster count given; using $CLUSTERS (sqrt of sample size)" 1>&2
      }

      clCount="$CLUSTERS"
      export clCount

      echo "Calculating SHA256 checksums of names" 1>&2
      echo "$INPUT" | "${haskellVersion}"
    '';
  };

  hashCheck = runCommand "hash-bucket-check"
    { buildInputs = [ fail hashes jq ]; }
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
withDeps [ hashCheck ] hashes
