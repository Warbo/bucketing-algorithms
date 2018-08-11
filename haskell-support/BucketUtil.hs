{-# LANGUAGE OverloadedStrings #-}
module BucketUtil where

import           Control.Monad       (mzero)
import qualified Data.Aeson          as A
import qualified Data.HashMap.Strict as HM
import qualified Data.Text           as T

newtype Name = Name { unName :: T.Text }

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

newtype Method = Method String

type Bucketer = (Method, Maybe Int -> [AST] -> [[AST]])

type Bucketed = HM.HashMap Method (HM.HashMap Int [[Name]])

bucketSizes :: [Int] -> [AST] -> Bucketer -> Bucketed
bucketSizes sizes asts (method, bucket) = HM.singleton method results
  where results = fromList (map go sizes)
        go size = (size, map (map getName) (bucket (Just size) asts))

-- For aggregated samples

newtype Sizes = Sizes (HM.HashMap Int -> Size)

newtype Size  = Size  (HM.HashMap Int -> Maybe Rep)

newtype Rep   = Rep { sample :: [Name], bucketed :: Bucketed }

instance A.ToJSON Rep where
  toJSON (Rep sample bucketed) =
    let meth (Method m, bucketed) = m .= object (map size (HM.toList bucketed))
        size (i, names)           = T.pack (show i) .= map (map unName) names
     in A.object (("sample" .= sample):map meth (HM.toList bucketed))

instance A.ToJSON Size where
  toJSON (Size m) = A.toJSON m

instance A.ToJSON Sizes where
  toJSON (Sizes m) = A.toJSON m

bucketAll :: [Bucketer] -> ([Name] -> [AST]) -> Sizes -> Sizes
bucketAll brs astsOf (Sizes ss) = Sizes (HM.map goSize ss)
  where goSize (Size s) = Size (HM.map goRep s)
        goRep r = case r of
          Nothing         -> Nothing
          Just (Rep s bs) -> Just (Rep s (HM.unions (bs:map (bucket s) brs)))
        bucket sample = bucketSizes [1..20] (astsOf sample)
