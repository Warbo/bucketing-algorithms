module BucketBounds where

import qualified BucketUtil                 as BU
import           Control.Monad.State.Strict (get, put, replicateM_, runState, State)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.IORef                 as IR
import           Data.Maybe                 (fromMaybe)
import           System.Environment         (lookupEnv)
import           System.IO                  (openFile, IOMode(ReadMode))
import           Test.QuickCheck
import           Test.Tasty                 (defaultMain, localOption, testGroup)
import           Test.Tasty.QuickCheck      (testProperty)

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

boundsTest = defaultMain $ testGroup "All tests" [
    checkProcess
  ]

checkProcess = testProperty "Can handle null reps" go
  where go = const False (process BU.testImp)
