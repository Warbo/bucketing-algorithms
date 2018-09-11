{-# LANGUAGE BangPatterns, OverloadedStrings, TemplateHaskell #-}
module GetGroundTruths where

import           Helper
import           Control.Monad              (mzero)
import qualified BucketUtil                 as BU
import           BucketUtil                 (getchar, info, putchar, putstr)
import qualified Data.Aeson                 as A
import qualified Data.AttoLisp              as L
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy       as LB
import qualified Data.Char                  as C
import qualified Data.HashMap.Strict        as H
import qualified Data.List                  as List
import qualified Data.Maybe                 as M
import qualified Data.Text                  as T
import           Language.Haskell.TH.Syntax (lift, runIO)
import qualified Numeric                    as N
import qualified System.Environment         as Env
import           System.IO.Unsafe           (unsafePerformIO)

theoremDeps :: [(TheoremID, [Name])]
theoremDeps =
  $(let name = "BENCHMARKS_THEOREM_DEPS"

        lispToData l = case L.parseEither L.parseLisp l of
                         Left  e -> error e
                         Right d -> d

        strToData :: B.ByteString -> [(TheoremID, [Name])]
        strToData  s = case AP.parseOnly (L.lisp <* AP.endOfInput) s of
                         Left  e -> error e
                         Right l -> lispToData l

     in do mp <- runIO (Env.lookupEnv name)
           s  <- case mp of
                   Nothing -> error ("Env doesn't contain " ++ name)
                   Just p  -> runIO (B.readFile p)
           lift (strToData s))

theoremFilesAdmittedBy :: [Name] -> [TheoremID]
theoremFilesAdmittedBy sample = map fst (filter match theoremDeps)
  where match td = snd td `subset` sample

mkResult :: LB.ByteString -> Result
mkResult = mkResult' . Right . M.fromJust . A.decode

sampleResult :: [Name] -> A.Value
sampleResult = sampleResultToJSON . mkResult' . Left

mkResult' :: Either [Name] [[Name]] -> Result
mkResult' ns = R { names = ns, theorems = either process nested ns }
  where process :: [Name] -> [TheoremID]
        process [] = []
        process ns = theoremFilesAdmittedBy (map decodeName ns)

        nested :: [[Name]] -> [TheoremID]
        nested []  = []
        nested nns = nub (concatMap process nns)

augmentSize :: Monad m => BU.StreamImp m -> m ()
augmentSize imp = BU.streamKeyVals' imp go
  where go key = do info imp ("Found rep " ++ show key)
                    putstr imp key
                    putstr imp " : "
                    augmentRep imp

-- We can't use streamKeyVals here, since reps may be null
augmentRep :: Monad m => BU.StreamImp m -> m ()
augmentRep imp = do c <- BU.skipSpace' imp
                    case c of
                      '[' -> do info imp "Rep is an array"
                                putchar imp '['
                                goArray imp
                      'n' -> do 'u' <- getchar imp
                                'l' <- getchar imp
                                'l' <- getchar imp
                                info imp "Rep is null"
                                putstr imp "null"
                      _   -> error (show (("Wanted", "'[' or 'null'"),
                                          ("Got",    c)))

goArray :: Monad m => BU.StreamImp m -> m ()
goArray imp = do -- First entry is {"sampleNames":[...]}
                 Just sampleStr <- BU.parseOne' imp
                 info imp ("First array elem is " ++ show sampleStr)
                 let Just obj    = A.decode sampleStr
                     Just !names = H.lookup "sampleNames"
                                     (obj :: H.HashMap String [Name])
                 info imp ("Array elem contains names " ++ show names)
                 putstr imp (A.encode (sampleResult names))
                 putchar imp ','

                 -- Second entry is {"method1":{...}, ...}
                 info imp "Looking for second elem (an object)"
                 '{' <- BU.skipSpace' imp
                 putchar imp '{'
                 go True
                 info imp "Closing off array"
                 ']' <- BU.skipSpace' imp
                 putchar imp ']'

  where go first = do info imp ("Looking for " ++ (if first
                                                      then "first"
                                                      else "next") ++ " key")
                      mk <- BU.parseOne' imp
                      case mk of
                        Nothing -> do info imp "No key found, closing off object"
                                      putchar imp '}' -- We've hit, and consumed, the '}'
                        Just !k -> do let key = BU.trimKey k
                                      info imp ("Found key " ++ show k)
                                      if first
                                         then pure ()
                                         else putchar imp ','
                                      f key
                                      go False

        f key = do putstr imp key
                   putstr imp " : "
                   info imp ("Processing " ++ show key ++ " as an object")
                   BU.streamKeyVals' imp (augmentRun imp)

findColon :: Monad m => BU.StreamImp m -> m ()
findColon imp = do c <- getchar imp
                   case c of
                     ':' -> pure ()
                     _ | C.isSpace c -> findColon imp
                     _ -> error ("Looking for ':' found " ++ show c)

augmentRun :: Monad m => BU.StreamImp m -> LB.ByteString -> m ()
augmentRun imp key = do info imp ("Found run " ++ show key)
                        putstr imp key
                        putstr imp " : "
                        findColon imp
                        Just str <- BU.parseOne' imp
                        info imp ("Value is " ++ show str)
                        putstr imp (A.encode (mkResult str))

main' imp = BU.streamKeyVals' imp go
  where go !key = do info imp ("Found size " ++ show key)
                     putstr imp key
                     putstr imp " : "
                     augmentSize imp

mainIO = main' BU.io
