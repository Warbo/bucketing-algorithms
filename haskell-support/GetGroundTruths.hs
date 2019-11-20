{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module GetGroundTruths where

import           BucketUtil                 (getchar, info, putchar, putstr)
import qualified BucketUtil                 as BU
import           Control.Monad              (mzero)
import qualified Data.Aeson                 as A
import qualified Data.AttoLisp              as L
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Char                  as C
import qualified Data.HashMap.Strict        as H
import qualified Data.List                  as List
import qualified Data.Maybe                 as M
import qualified Data.Text                  as T
import           Helper
import           Language.Haskell.TH.Syntax (lift, runIO)
import qualified Numeric                    as N
import qualified System.Environment         as Env
import           System.IO.Unsafe           (unsafePerformIO)

-- | Database of theorem dependencies, read at compile time. These are in two
--   forms: theoremDeps are the human-readable names; encodedDeps are
--   hex-encoded. The latter are directly comparable to sampled names, which
--   prevents having to do a bunch of decoding over and over.
theoremDeps, encodedDeps :: [(TheoremID, AscendingList Name)]
(theoremDeps, encodedDeps) = (map wrapSecond unwrapped, map wrapSecond encoded)
  where -- At run time we convert each (already sorted) list of deps into an
        -- 'AscendingList'. This is a newtype so there's minimal overhead.
        wrapSecond (t, deps) = (t, AscendingList deps)

        -- Build the datastructure at compile time, including sorting the deps
        -- into ascending order. There's no 'Lift' instance for 'AscendingList'
        -- so we leave them as regular lists.
        (unwrapped, encoded) =
          $(let name = "BENCHMARKS_THEOREM_DEPS"

                lispToData l = case L.parseEither L.parseLisp l of
                                 Left  e -> error e
                                 Right d -> d

                strToData :: B.ByteString -> [(TheoremID, [Name])]
                strToData  s = case AP.parseOnly (L.lisp <* AP.endOfInput) s of
                                 Left  e -> error e
                                 Right l -> lispToData l

                sortSecond f (t, deps) = (t, List.sort (map f deps))

                sorted f = map (sortSecond f)

             in do mp <- runIO (Env.lookupEnv name)
                   s  <- case mp of
                           Nothing -> error ("Env doesn't contain " ++ name)
                           Just p  -> runIO (B.readFile p)
                   let d = strToData s
                   lift (sorted id d, sorted encodeName d))

theoremFilesAdmittedBy :: AscendingList Name -> [TheoremID]
theoremFilesAdmittedBy = theoremFilesAdmittedBy' encodedDeps

theoremFilesAdmittedBy' :: [(TheoremID, AscendingList Name)]
                        -> AscendingList Name
                        -> [TheoremID]
theoremFilesAdmittedBy' db sample = map fst (filter match db)
  where match td = snd td `subsetAsc` sample

mkResult :: LB.ByteString -> Result
mkResult = mkResult' . Right . M.fromJust . A.decode

sampleResult :: [Name] -> A.Value
sampleResult = sampleResultToJSON . mkResult' . Left

mkResult' :: Either [Name] [[Name]] -> Result
mkResult' ns = R { names = ns, theorems = either process nested ns }
  where process :: [Name] -> [TheoremID]
        process [] = []
        process ns = theoremFilesAdmittedBy (mkAscendingList ns)

        nested :: [[Name]] -> [TheoremID]
        nested []  = []
        nested nns = nub (concatMap process nns)

augmentSize :: Monad m => BU.StreamImp m -> m ()
augmentSize imp = BU.streamKeyVals imp go
  where go key = do putstr imp key
                    putstr imp " : "
                    augmentRep imp

-- We can't use streamKeyVals here, since reps may be null
augmentRep :: Monad m => BU.StreamImp m -> m ()
augmentRep imp = do c <- BU.skipSpace imp
                    case c of
                      '[' -> do putchar imp '['
                                goArray imp
                      'n' -> do 'u' <- getchar imp
                                'l' <- getchar imp
                                'l' <- getchar imp
                                putstr imp "null"
                      _   -> error (show (("Wanted", "'[' or 'null'"),
                                          ("Got",    c)))

goArray :: Monad m => BU.StreamImp m -> m ()
goArray imp = do -- First entry is {"sampleNames":[...]}
                 Just sampleStr <- BU.parseOneChunked' imp
                 let Just obj    = A.decode sampleStr
                     Just !names = H.lookup "sampleNames"
                                     (obj :: H.HashMap String [Name])
                 putstr imp (A.encode (sampleResult names))
                 putchar imp ','

                 -- Second entry is {"method1":{...}, ...}
                 '{' <- BU.skipSpace imp
                 putchar imp '{'
                 go True
                 ']' <- BU.skipSpace imp
                 putchar imp ']'

  where go first = do
          mk <- BU.parseOneChunked' imp
          case mk of
            Nothing -> do putchar imp '}' -- We've hit, and consumed, the '}'
            Just !k -> do let key = BU.trimKey k
                          if first
                             then pure ()
                             else putchar imp ','
                          f key
                          go False

        f key = do putstr imp key
                   putstr imp " : "
                   BU.streamKeyVals imp (augmentRun imp)

findColon :: Monad m => BU.StreamImp m -> m ()
findColon imp = do c <- getchar imp
                   case c of
                     ':' -> pure ()
                     _ | C.isSpace c -> findColon imp
                     _ -> error ("Looking for ':' found " ++ show c)

augmentRun :: Monad m => BU.StreamImp m -> LB.ByteString -> m ()
augmentRun imp key = do putstr imp key
                        putstr imp " : "
                        findColon imp
                        Just str <- BU.parseOneChunked' imp
                        putstr imp (A.encode (mkResult str))

main' imp = BU.streamKeyVals imp go
  where go !key = do mapM_ (info imp) ["Found size ", LB.unpack key, "\n"]
                     putstr imp key
                     putstr imp " : "
                     augmentSize imp

-- | Read stdin into a lazy buffer then stream from it
mainIO = do imp <- BU.buf
            BU.runBIO (main' imp)
