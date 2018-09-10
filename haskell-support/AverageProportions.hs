{-# LANGUAGE BangPatterns, OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict      as Map
import qualified Data.MessagePack     as MP
import           Data.MessagePack     (Object(..))
import           Data.String          (fromString, IsString)
import qualified Data.Text            as T

instance IsString Object where
  fromString = ObjectStr . T.pack

err :: Show a => a -> b
err = error . show

dropDupes = filter notNil
  where notNil (k, ObjectNil) = False
        notNil (k, _        ) = True

process :: Object -> Object
process x = case x of
    ObjectMap kvs -> ObjectMap (map go kvs)
    _             -> err (("error", "'process' expected map"),
                          ("given", x                       ))
  where go (k, v) = (k, processSize v)

type BucketSize = Int
type Proportion = Double
type Buckets    = Map.Map BucketSize [Proportion]
type Method     = T.Text
type Methods    = Map.Map Method Buckets

-- | Iterate through reps, accumulating a map of average data for each method.
--   Write out the map of method->bucketSize->stats when done.
processSize :: Object -> Object
processSize (ObjectMap rs) = methodsToObjects (gatherReps Map.empty rs)

-- | Iterates through reps of method->bucketSizeMap, discarding nulls and
--   accumulating a method->bucketSize->[proportion] of the rest
gatherReps :: Methods -> [(Object, Object)] -> Methods
gatherReps !acc kvs = case kvs of
                        []            -> acc
                        (_, rep):kvs' -> gatherReps (withRep rep) kvs'
  where withRep x = case x of
          ObjectNil                            -> acc
          (ObjectArray [_, ObjectMap methods]) -> Map.unionWith mergeReps
                                                                acc
                                                                (repMap methods)

methodsToObjects :: Methods -> Object
methodsToObjects = process wrapMethod
  where process f = ObjectMap . map f . Map.toList

        wrapMethod :: (Method, Buckets) -> (Object, Object)
        wrapMethod (          method,                    buckets) =
                   (ObjectStr method, process wrapBucket buckets)

        wrapBucket :: (BucketSize, [Proportion]) -> (Object, Object)
        wrapBucket (                        bucketSize  ,             ps ) =
                   (ObjectStr (showText bucketSize), ObjectMap (stats ps))

        showText = T.pack . show

stats :: [Proportion] -> [(Object, Object)]
stats props = [("proportion", ObjectMap [("mean"  , ObjectDouble mean  ),
                                         ("stddev", ObjectDouble stddev)])]
  where -- Calculates values for mean and variance in one pass
        go :: (Double, Double, Double) -> [Proportion]
           -> (Double, Double, Double)
        go !acc                 []      = acc
        go (!cnt, !tot, !sqTot) (p:ps)  = go (cnt+1, tot+p, sqTot + (p*p)) ps

        (!count, !total, !squaredTotal) = go (0    , 0    , 0            ) props

        -- Mean is easy
        !mean = total / count

        -- Variance is defined as:
        --
        --     sum(map (\n -> (n - mean)^2) xs) / (count - 1)
        --
        -- We can do the division at the end, and it's easy enough to sum in one
        -- pass. The problem is using mean, which we won't know until the end.
        -- If we expand the call to map:
        --
        --     sum [(x1 - mean)^2            , (x2 - mean)^2            , ...]
        --     sum [(x1 - mean) * (x1 - mean), (x2 - mean) * (x2 - mean), ...]
        --     sum [x1^2 - 2*x1*mean + mean^2, x2^2 - 2*x2*mean + mean^2, ...]
        --
        -- We can factor out occurrences of 'mean', to get:
        --
        --     sum (map (^2) xs) + 2 * mean * sum xs + mean^2 * length xs
        --
        -- We calculate the count, total and squaredTotal in one pass above.
        !variance = let top = squaredTotal + (2 * mean * total) + (mean * mean)
                     in top / (count - 1)

        !stddev = sqrt variance

mergeReps :: Buckets -> Buckets -> Buckets
mergeReps = Map.unionWith (++)

-- | Iterates through a rep of method->bucketSizeMap, plucking a result for each
--   of the method's bucket sizes.
repMap :: [(Object, Object)] -> Methods
repMap = go Map.empty
  where go !acc []                                              = acc
        go !acc ((ObjectStr method, ObjectMap buckets):methods) =
          go (Map.insert (method              :: Method)
                         ((bucketMap buckets) :: Buckets)
                         (acc                 :: Methods))
             methods

-- | Collects up the ground truth proportions from a map of bucketSize->results
bucketMap :: [(Object, Object)] -> Buckets
bucketMap = Map.fromList . map go
  where go (ObjectStr      bucketSize , ObjectMap info ) =
           (read (T.unpack bucketSize), [prop     info])

-- | Given the results for a particular bucket size, extracts the proportion of
--   ground truth theorems which are possible to find.
prop :: [(Object, Object)] -> Proportion
prop l = case lookup "comparison" l of
           Just (ObjectMap l') -> case lookup "proportion" l' of
                                    Just (ObjectDouble p) -> p

                                    -- Error handling
                                    Just x  -> err (("error", "Not Double"),
                                                    ("proportion", x      ))
                                    Nothing -> err (("error", "Not found" ),
                                                    ("key"  , "proportion"),
                                                    ("data" , l'          ))
           -- Error handling
           Just x  -> err (("error"     , "Not Map"   ),
                           ("comparison", x           ))
           Nothing -> err (("error"     , "Not found" ),
                           ("key"       , "comparison"),
                           ("entries"   , l           ))

main :: IO ()
main = BS.interact (MP.pack . process . unwrap . MP.unpack)
  where unwrap Nothing  = error "Couldn't decode MsgPack from stdin"
        unwrap (Just x) = x
