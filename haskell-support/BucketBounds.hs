{-# LANGUAGE FlexibleContexts, OverloadedStrings, PartialTypeSignatures #-}
module BucketBounds where

import qualified Control.Lens      as Lens
import           Control.Lens.Operators
import           Control.Lens.Type (IndexedTraversal', Lens')
import           Data.Aeson        (toJSON)
import           Data.Aeson.Lens

{-
import qualified BucketUtil                 as BU
import           Control.Monad.State.Strict (get, put, replicateM_, runState, State)
-}
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Char                  as C
import           Data.List                  (intercalate)
import qualified Data.Map.Strict            as Map
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import qualified Data.Vector.Lens           as V
{-
import qualified Data.IORef                 as IR
import           Data.Maybe                 (fromMaybe)
import           System.Environment         (lookupEnv)
import           System.IO                  (openFile, IOMode(ReadMode))
-}
import           Test.QuickCheck
import           Test.Tasty                 (defaultMain, localOption, testGroup)
import           Test.Tasty.QuickCheck      (testProperty)

type Size      = Int
type Name      = T.Text
type Sample    = V.Vector Name
type RawSample = V.Vector A.Value

{-
boundsMain :: IO ()
boundsMain = do
  jsonFileM <- lookupEnv "SAMPLE_JSON"
  let jsonFile = fromMaybe (error "No SAMPLE_JSON env var given, aborting")
                           jsonFileM
  file    <- openFile jsonFile ReadMode
  content <- BS.hGetContents file
  imp     <- bsImp content
  process imp

-- Turns {Size: {Rep: Sample}} into {Size: {Rep: {Count: Proportion}}}
process :: Monad m => BU.StreamImp m -> m ()
process imp = BU.streamKeyVals' imp (processSize imp)

-- Turns (Size, {Rep: Sample}) into {Rep: {Count: Proportion}}
processSize :: Monad m => BU.StreamImp m -> BS.ByteString -> m ()
processSize imp sizeStr = BU.streamKeyVals' imp (processRep imp size)
  where size :: Int
        size = read (BS.unpack sizeStr)

-- Turns (Size, Sample) into {Count: Proportion}
processRep :: Monad m => BU.StreamImp m -> Int -> BS.ByteString -> m ()
processRep imp size repStr = BU.streamKeyVals' imp (processSample size)

processSample = undefined

bsImp :: BS.ByteString -> IO (BU.StreamImp IO)
bsImp = fmap mkImp . IR.newIORef
  where mkImp ref = BU.StreamImp {
          BU.getchar     = do c <- fmap BS.head (IR.readIORef ref)
                              IR.modifyIORef' ref BS.tail
                              pure c,

          BU.getcontents = IR.readIORef ref,
          BU.info        = BU.debug,
          BU.putchar     = putChar,
          BU.putstr      = BS.putStr
        }

prop_processHandlesJSON = testProperty "JSON is handled by process"
                                       (forAll genSizes goSizes)

  where goSizes ss    = forAll (genFromSizes ss) (go ss)
        go sizes json = undefined (BU.runOn :: _) False (process BU.testImp)

genSizes :: Gen [Int]
genSizes = listOf (choose 1 20)

genFromSizes :: [Int] -> Gen BS.ByteString
genFromSizes = pure . A.toJSON . map go
  where go :: [Int] -> [(Int, ())]
        go s = (s, ())
-}

--_sample :: Traversal' t Sample
{-_sample = members      . _Object .  -- Each size entry
          members      . _Object .  -- Each rep entry
          key "sample" . _Array  .  -- The sample (array of names)
          from V.vector          .  -- Convert to a list
          each           _String    -- Those names which are strings
-}

--_sizes :: _
--_sizes = members . _Object

--"{\"a\": 4, \"b\": 7}" & members . _Number *~ 10

decodeSample :: Lens.Lens' RawSample Sample
decodeSample = Lens.lens decode encode
  where decode   = V.map (^. _String)
        encode _ = V.map A.String

_sampleKey :: _ --Lens.Lens' String Sample
_sampleKey = key "sample" . _Array . decodeSample

--inputByteString & key "x1" . key "x2" . _Array %~ sum

boundsMain = error "NOT IMPLEMENTED"

boundsTest = defaultMain $ testGroup "All tests" [
    prop_getSampleKey
    --prop_getSizes
    --prop_getSample
  ]

prop_getSampleKey = testProperty "Can look up 'sample' from object" go
  where go names = canGetSample names (mkObj names)

        mkObj :: Sample -> String
        mkObj names = let rendered = map show (V.toList names)
                          list     = "[" ++ intercalate ", " rendered ++ "]"
                       in "{\"sample\":" ++ list ++ "}"

        sampledNames :: String -> Maybe Sample
        sampledNames = (^? _sampleKey)

        canGetSample :: Sample -> String -> Property
        canGetSample names str = let got  = sampledNames str
                                     want = Just names
                                  in got === want


--prop_getSizes = _sample "{\"1\":{\"2\":{\"sample\":[\"foo\"]}}" == [["foo"]]

{-
prop_getSample = testProperty "Can get samples" go
  where go :: Int -> Sample -> Bool
        go reps names = let reps' = abs reps + 1
                            names' = filter (not . null) names
                         in check reps' names' (render reps' names' [])

        check :: Int -> Sample -> String -> Bool
        check reps names str = str & _sample

        render reps []       acc = toJSON (Map.fromList acc)
        render reps (ns:nss) acc = render reps nss (mkReps ns reps []:acc)

        mkReps ns 0    acc = (length ns, acc)
        mkReps ns reps acc = mkReps (rotate ns) (reps - 1) (ns:acc)

rotate []     = []
rotate (x:xs) = xs ++ [x]
-}

instance Arbitrary T.Text where
  arbitrary = T.pack . filter ok <$> arbitrary
    where ok c = C.isAlphaNum c && C.isAscii c

instance Arbitrary a => Arbitrary (V.Vector a) where
  arbitrary = V.fromList <$> listOf arbitrary
  shrink    = map V.fromList . shrink . V.toList
