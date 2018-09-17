{-# LANGUAGE BangPatterns, OverloadedStrings #-}

import           Control.Exception             (assert)
import qualified Data.Binary.Get               as BG
import qualified Data.Binary.Put               as BP
import           Data.Bits                     ((.&.))
import qualified Data.ByteString.Lazy          as BS
import qualified Data.ByteString.Lazy.Internal as BS
import           Data.List                     (sort)
import           Data.Maybe                    (fromJust, isNothing)
import qualified Data.MessagePack              as MP
import           Data.MessagePack              (Object(..))
import           Data.String                   (fromString, IsString)
import qualified Data.Text                     as T
import           MsgPack

-- We define a few types to spot obvious errors. We mostly deal with ByteString
-- reading and writing, rather than "proper" Haskell datatypes, since this makes
-- our streaming behaviour more explicit; yet this makes everything "stringly
-- typed" (i.e. strongly typed, but all the types are just ByteString). We add
-- phantom types to 'tag' our functions with what they expect to encode/decode
-- from the ByteStrings.


-- Types to use for our Consumers

data Size
data Rep
data BucketSize
data Sample = Sample { sNames :: [MP.Object], sTheorems :: [MP.Object] }
data Method
data RawBucket
data BucketWithProp
data MsgPackNull

type RepData          = Either MsgPackNull
type RawRep           = (Sample, RawMethods      )
type RepWithProps     = (Sample, MethodsWithProps)
type RawSize          = MsgPackMap Rep        (RepData RawRep      )
type SizeWithProps    = MsgPackMap Rep        (RepData RepWithProps)
type RawMethods       = MsgPackMap Method     RawBuckets
type MethodsWithProps = MsgPackMap Method     BucketsWithProps
type RawBuckets       = MsgPackMap BucketSize RawBucket
type BucketsWithProps = MsgPackMap BucketSize BucketWithProp

-- Helpers

sampleMP :: Sample -> MP.Object
sampleMP (Sample names theorems) = MP.ObjectMap [
    ("sampleNames"   , MP.ObjectArray names   ),
    ("sampleTheorems", MP.ObjectArray theorems)
  ]

getSample :: BG.Get Sample
getSample = do
    MP.ObjectMap o <- MP.getObject
    case (lookup "sampleNames"    o,
          lookup "sampleTheorems" o) of
         (Just (MP.ObjectArray names   ),
          Just (MP.ObjectArray theorems)) -> pure (Sample {
                                                    sNames    = names,
                                                    sTheorems = theorems
                                                  })
         _ -> error "Failed to read sample"
  where unStr (MP.ObjectStr x) = x

-- Data processors, one for each 'level'

process :: Consumer (MsgPackMap Size RawSize) (MsgPackMap Size SizeWithProps)
process = mapMap "top-level" (processSize)

processSize :: Consumer (MsgPackKeyVal Size RawSize      )
                        (MsgPackKeyVal Size SizeWithProps)
processSize = dumpKey "processSize"
                      (mapMap "processSize"
                              (dumpKey "processRepData" processRepData))

processRepData :: Consumer (RepData RawRep) (RepData RepWithProps)
processRepData = Consumer go
  where go bs = case parseNext "processSize" (getRepHeader "processSize") bs of
                  (RepTypeNull, bs') -> writeMP MP.ObjectNil >> pure bs'
                  (RepTypeData, bs') -> consume processRep bs'

processRep :: Consumer RawRep RepWithProps
processRep =  Consumer go
  where go bs = let (sample, bs') = parseNext "processRep" getSample bs
                 in do BS.putStr (BS.pack [fromIntegral 0x92])
                       writeMP (sampleMP sample)
                       consume (processMethods sample) bs'

processMethods :: Sample -> Consumer RawMethods MethodsWithProps
processMethods sample = mapMap "processMethods"
                               (dumpKey "processMethods" (processMethod sample))

processMethod :: Sample -> Consumer RawBuckets BucketsWithProps
processMethod sample = mapMap "processMethod"
                              (dumpKey "processMethod" (processBucket sample))

processBucket :: Sample -> Consumer RawBucket BucketWithProp
processBucket sample = Consumer go
  where go bs = let (bucket, bs') = parseNext "processBucket" MP.getObject bs
                 in writeMP (bucketProportion sample bucket) >> pure bs'

bucketProportion (Sample sampleNames sampleTheorems) (MP.ObjectMap bucket) =
    check (MP.ObjectDouble proportion)
  where Just (ObjectArray names   ) = lookup "names"    bucket
        Just (ObjectArray theorems) = lookup "theorems" bucket
        found     = length       theorems
        available = length sampleTheorems

        check = namesMatch . theoremsMatch . lowerCheck . upperCheck

        bucketNames = concatMap unArray names

        assert s c x = if c then x else err s
        namesMatch   = let b = sort bucketNames
                           s = sort sampleNames
                        in assert (("error" , "Bucket/sample name mismatch"),
                                   ("bucket", b                            ),
                                   ("sample", s                            ))
                                  (b == s)

        theoremsMatch = assert (("error" , "Bucket theorem not in sampled"),
                                ("bucket", theorems                       ),
                                ("sample", sampleTheorems                 ))
                               (all (`elem` sampleTheorems) theorems)

        proportion = fromIntegral found / fromIntegral available

        lowerCheck = assert (("error"     , "Proportion less than 0"),
                             ("proportion", proportion              ))
                            (proportion >= 0)
        upperCheck = assert (("error"     , "Proportion more than 1"),
                             ("proportion", proportion              ))
                            (proportion <= 1)

main = msgPackMain process
