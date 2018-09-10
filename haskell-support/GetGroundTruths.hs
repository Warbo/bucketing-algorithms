{-# LANGUAGE BangPatterns, OverloadedStrings, TemplateHaskell #-}
module GetGroundTruths where

import           Helper
import           Control.Monad              (mzero)
import qualified BucketUtil                 as BU
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
import           System.IO                  (hPutStrLn, stderr)
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

augmentSize :: IO ()
augmentSize = BU.streamKeyVals go
  where go key = do --info ("Found rep " ++ show key)
                    LB.putStr key
                    LB.putStr " : "
                    augmentRep

-- We can't use streamKeyVals here, since reps may be null
augmentRep :: IO ()
augmentRep = do c <- BU.skipSpace
                case c of
                  '[' -> do --info "Rep is an array"
                            putChar '['
                            goArray
                  'n' -> do 'u' <- getChar
                            'l' <- getChar
                            'l' <- getChar
                            --info "Rep is null"
                            LB.putStr "null"
                  _   -> error (show (("Wanted", "'[' or 'null'"),
                                      ("Got",    c)))

goArray = do -- First entry is {"sampleNames":[...]}
             Just sampleStr <- BU.parseOne
             --info ("First array elem is " ++ show sampleStr)
             let Just obj    = A.decode sampleStr
                 Just !names = H.lookup "sampleNames"
                                 (obj :: H.HashMap String [Name])
             --info ("Array elem contains names " ++ show names)
             LB.putStr (A.encode (sampleResult names))
             putChar ','

             -- Second entry is {"method1":{...}, ...}
             --info "Looking for second elem (an object)"
             '{' <- BU.skipSpace
             putChar '{'
             go True
             --info "Closing off array"
             putChar ']'

  where go first = do --info ("Looking for " ++ (if first
                      --                            then "first"
                      --                            else "next") ++ " key")
                      mk <- BU.parseOne
                      case mk of
                        Nothing -> do --info "No key found, closing off object"
                                      putChar '}' -- We've hit, and consumed, the '}'
                        Just !k -> do let key = BU.trimKey k
                                      --info ("Found key " ++ show k)
                                      if first
                                         then pure ()
                                         else putChar ','
                                      f key
                                      go False

        f key = do LB.putStr key
                   LB.putStr " : "
                   --info ("Processing " ++ show key ++ " as an object")
                   BU.streamKeyVals augmentRun

findColon = do c <- getChar
               case c of
                 ':' -> pure ()
                 _ | C.isSpace c -> findColon
                 _ -> error ("Looking for ':' found " ++ show c)

augmentRun :: LB.ByteString -> IO ()
augmentRun key = do --info ("Found run " ++ show key)
                    LB.putStr key
                    LB.putStr " : "
                    findColon
                    Just str <- BU.parseOne
                    --info ("Value is " ++ show str)
                    LB.putStr (A.encode (mkResult str))

main' imp = streamKV imp go
  where go key = do info imp ("Found size " ++ show key)
                    putstr imp key
                    putstr imp " : "
                    augmentSize' imp

data Imp m = Imp {
    info   ::        String -> m ()
  , putstr :: LB.ByteString -> m ()
  }

io = Imp {
    info     = hPutStrLn stderr
  , putstr   = LB.putStr
  , streamKV = BU.streamKeyVals
  }
