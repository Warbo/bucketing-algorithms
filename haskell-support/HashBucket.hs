{-# LANGUAGE OverloadedStrings #-}
module HashBucket where
import           BucketUtil
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
import qualified Data.Text.Encoding         as TE
import           System.Environment         (lookupEnv)
import           System.IO.Unsafe           (unsafePerformIO)

newtype Input = Input { unInput :: [AST] }

instance A.FromJSON Input where
  parseJSON j = case j of
    A.Object _ -> do ast  <- A.parseJSON j
                     pure (Input [ast])
    A.Array  _ -> do asts <- A.parseJSON j
                     pure (Input asts)
    _          -> mzero

bucket :: Int -> [AST] -> [[AST]]
bucket cs asts = go (Map.fromList [(i - 1, []) | i <- [1..cs]]) asts
  where go acc []     = Map.elems acc
        go acc (a:as) = let (c, a') = pickBucket (toInteger cs) a
                         in go (addToBucket c a' acc) as

type BucketMap = Map.Map Int [AST]

addToBucket :: Int -> AST -> BucketMap -> BucketMap
addToBucket i v m = Map.alter insert i m
  where insert Nothing   = Just [v]
        insert (Just vs) = Just (v:vs)

pickBucket :: Integer -> AST -> (Int, AST)
pickBucket clusters x = (cluster, x { getAST  = ast' })
  where cluster :: Num a => a
        cluster = fromInteger (num `mod` clusters)
        name    = getName x
        ast     = getAST  x
        ast'    = HM.insert "cluster" (A.Number (1 + cluster)) ast
        num     = bsToInteger hash
        hash    = convert (H.hashWith H.SHA256 (TE.encodeUtf8 name))

bsToInteger :: BS.ByteString -> Integer
bsToInteger = BS.foldl appendByte 0
  where appendByte n b = (n * 256) + toInteger b

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
