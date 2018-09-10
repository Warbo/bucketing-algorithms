{-# LANGUAGE BangPatterns, OverloadedStrings #-}

import           Control.Exception          (assert)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.List                  (sort)
import           Data.Maybe                 (fromJust, isNothing)
import qualified Data.MessagePack           as MP
import           Data.MessagePack           (Object(..))
import           Data.String                (fromString, IsString)
import           Data.Ratio                 ((%))
import qualified Data.Text                  as T

-- Helpers

instance IsString Object where
  fromString = ObjectStr . T.pack

err :: Show a => a -> b
err = error . show

mapMap label f x = case x of
    ObjectMap kvs -> ObjectMap (map go kvs)
    _             -> err (("error", "Expected map"),
                          ("function", label      ),
                          ("got"  , x             ))
  where go arg@(k, v) = (k, f arg)

-- Data processors, one for each 'level'

process = mapMap "process" go
  where go (key, size) = processSize size

processSize = mapMap "processSize" go
  -- Some reps are None ('null' in JSON) since they're dupes. Skip them.
  where go (k, rep) = case rep of
          ObjectNil                                             -> ObjectNil
          ObjectArray [s@(ObjectMap sample), ObjectMap methods] ->
            case (lookup "sampleNames"    sample,
                  lookup "sampleTheorems" sample) of
              -- Happy path
              (Just (ObjectArray names   ),
               Just (ObjectArray theorems)) -> ObjectArray [
                s,
                ObjectMap (mapSnd (processMethod names theorems) methods)]

              -- Missing or malformed data
              (names, theorems) -> err
                (("errors", concat [
                  if isNothing names
                     then ["No 'sampleNames' found"]
                     else if isArr names
                             then []
                             else ["'sampleNames' should be array"],
                  if isNothing theorems
                     then ["No 'sampleTheorems' found"]
                     else if isArr theorems
                             then []
                             else ["'sampleTheorems' should be array"]]),

                 ("sampleNames"   , names   ),
                 ("sampleTheorems", theorems),
                 ("sample"        , sample  ))

          _ -> error ("'processSize' expected pair, got " ++ show rep)

        isArr (Just (ObjectArray _)) = True
        isArr _                      = False

        mapSnd :: (b -> c) -> [(a, b)] -> [(a, c)]
        mapSnd f = map (\(k, v) -> (k, f v))

processMethod :: [Object] -> [Object] -> Object -> Object
processMethod names theorems = mapMap "processMethod" go
  where go (k, v) = processBuckets names theorems v

processBuckets :: [Object] -> [Object] -> Object -> Object
processBuckets sampleNames sampleTheorems (ObjectMap kvs) = checked
  where !checked = check result
        check    = namesMatch . theoremsMatch . lowerCheck . upperCheck
        result   = ObjectMap (("comparison", comparison) : kvs)

        Just (ObjectArray names   ) = lookup "names"    kvs
        Just (ObjectArray theorems) = lookup "theorems" kvs
        bucketNames                 = concatMap unArray names

        assert s c x  = if c then x else err s
        namesMatch    = let b = sort bucketNames
                            s = sort sampleNames
                         in assert (("error" , "Bucket/sample name mismatch"),
                                    ("bucket", b                            ),
                                    ("sample", s                            ))
                                   (b == s)
        theoremsMatch = assert (("error" , "Bucket theorem not in sampled"),
                                ("bucket", theorems                       ),
                                ("sample", sampleTheorems                 ))
                               (all (`elem` sampleTheorems) theorems)

        found      = length       theorems
        available  = length sampleTheorems
        proportion = fromIntegral found / fromIntegral available
        lowerCheck = assert (("error"     , "Proportion less than 0"),
                             ("proportion", proportion              ))
                            (proportion >= 0)
        upperCheck = assert (("error"     , "Proportion more than 1"),
                             ("proportion", proportion              ))
                            (proportion <= 1)

        int = ObjectInt . fromIntegral
        comparison = ObjectMap [("found"     , int found              ),
                                ("available" , int available          ),
                                ("proportion", ObjectDouble proportion),
                                ("missing"   , int (available - found))]

unArray (ObjectArray a) = a
unArray x               = error ("Expected array, got " ++ show x)

main = BS.interact (MP.pack . process . unwrap . MP.unpack)
  where unwrap Nothing  = error "Couldn't decode MsgPack from stdin"
        unwrap (Just x) = x
