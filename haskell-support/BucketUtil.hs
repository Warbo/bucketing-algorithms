{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
module BucketUtil where

import           Control.Applicative ((<|>))
import           Control.Monad       (mzero)
import qualified Data.Aeson          as A
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict     as Map
import           Data.Maybe          (fromJust)
import qualified Data.String         as S
import qualified Data.Text           as T
import           System.Environment  (lookupEnv)
import           System.IO.Unsafe    (unsafePerformIO)

newtype Name = Name { unName :: T.Text }

instance A.FromJSON Name where
  parseJSON j = Name <$> A.parseJSON j

instance A.ToJSON Name where
  toJSON (Name n) = A.toJSON n

data AST = AST {
  getName :: Name,
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

-- Gets number of clusters to use, taking
clusters :: [a] -> Int
clusters asts = fromJust (fromSize <|> fromEnv <|> Just fromIn)
  where fromSize = case (unsafePerformIO (lookupEnv "CLUSTER_SIZE")) of
                     Nothing -> Nothing
                     Just s  -> let size = fromIntegral (read s :: Int)
                                    len  = fromIntegral inCount :: Float
                                 in Just (ceil (len / size))
        fromEnv = fmap read (unsafePerformIO (lookupEnv "CLUSTERS"))
        fromIn  = ceil (sqrt (fromIntegral inCount))
        inCount = length asts
        ceil    = ceiling :: Float -> Int

newtype Method = Method T.Text deriving (Eq, Ord)

type Bucketer = (Method, Maybe Int -> [AST] -> [[AST]])

type Bucketed = Map.Map Method (Map.Map Int [[Name]])

bucketSizes :: [Int] -> [AST] -> Bucketer -> Bucketed
bucketSizes sizes asts (method, bucket) = Map.singleton method results
  where results = Map.fromList (map go sizes)
        go size = (size, map (map getName) (bucket (Just size) asts))

-- For aggregated samples

toJSON' m = let convert (k, v) = T.pack (show k) A..= v
             in A.object (map convert (Map.toList m))

newtype Sizes = Sizes (Map.Map Int Size)

newtype Size  = Size  (Map.Map Int (Maybe Rep))

data    Rep   = Rep { sample :: [Name], bucketed :: Bucketed }

instance A.ToJSON Rep where
  toJSON (Rep sample bucketed) =
    let meth (Method m, bucketed) = m A..= A.object (map size (Map.toList bucketed))
        size (i, names)           = T.pack (show i) A..= map (map unName) names
     in A.object (("sample" A..= sample):map meth (Map.toList bucketed))

instance A.ToJSON Size where
  toJSON (Size m) = toJSON' m

instance A.ToJSON Sizes where
  toJSON (Sizes m) = toJSON' m

bucketAll :: [Bucketer] -> ([Name] -> [AST]) -> Sizes -> Sizes
bucketAll brs astsOf (Sizes ss) = Sizes (Map.map goSize ss)
  where goSize (Size s) = Size (Map.map goRep s)
        goRep r = case r of
          Nothing         -> Nothing
          Just (Rep s bs) -> Just (Rep s (Map.unions (bs:map (bucket s) brs)))
        bucket sample = bucketSizes [1..20] (astsOf sample)
