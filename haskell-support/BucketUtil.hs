{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections         #-}
module BucketUtil where

import           Control.Applicative        ((<|>))
import           Control.DeepSeq            (($!!), deepseq, force, NFData, rnf)
import           Control.Monad              (mzero, replicateM)
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Char                  as C
import qualified Data.HashMap.Strict        as HM
import qualified Data.Interned              as I
import qualified Data.Interned.Text         as IT
import qualified Data.List                  as L
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (fromJust)
import qualified Data.String                as S
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Encoding    as TE
import           Debug.Trace                (trace)
import           GHC.Generics               (Generic)
import qualified HS2AST.Types               as HT
import           System.Environment         (lookupEnv)
import           System.IO                  (hPutStrLn, stderr)
import           System.IO.Unsafe           (unsafePerformIO)

newtype Name = Name IT.InternedText

instance NFData Name where
  rnf (Name !n) = ()

mkName !t = let !t' = T.copy   t
                !i  = I.intern t'
             in Name $! i

unName (Name !n) = I.unintern $! n

instance A.FromJSON Name where
  parseJSON j = mkName <$> A.parseJSON j

instance A.ToJSON Name where
  toJSON (Name n) = A.toJSON (I.unintern n)

data AST = AST {
  getName :: Name,
  getAST  :: A.Object,
  keeper  :: !Bool
} deriving Generic

instance NFData AST

instance A.ToJSON AST where
  toJSON = A.Object . getAST

instance A.FromJSON AST where
  parseJSON (A.Object o) = do
    n <- o A..:  "name"
    t <- o A..:? "type"
    q <- o A..:? "quickspecable"
    pure $! force (AST {
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

newtype Method = Method { unMethod :: T.Text } deriving (Eq, Ord)

instance NFData Method where
  rnf m = unMethod m `deepseq` ()

instance A.ToJSON Method where
  toJSON (Method m) = A.toJSON m

instance A.FromJSON Method where
  parseJSON j = Method <$> A.parseJSON j

type Bucketer = (Method, Int -> [AST] -> [[HT.Identifier]])

type Bucketed = Map.Map Method (Map.Map Int [[Name]])

bucketCounts :: [Int] -> [AST] -> Bucketer -> Bucketed
bucketCounts counts asts (method, bucket) = force (Map.singleton method results)
  where results = Map.fromList (map go counts)
        go count = let buckets = map (map (mkName . HT.idName)) (bucket count asts)
                   in count `deepseq` buckets `deepseq` (count, buckets)

entries :: Bucketed -> [Map.Map Int [[Name]]]
entries = map snd . Map.toList

-- For aggregated samples

toJSON' m = let convert (k, v) = force (T.pack (show k)) A..= v
             in A.object (map convert (Map.toList m))

data    Rep   = Rep { sample :: [Name], bucketed :: Bucketed }

instance NFData Rep where
  rnf r = sample r `deepseq` bucketed r `deepseq` ()

instance A.ToJSON Rep where
  toJSON (Rep sample bucketed) =
    let meth (Method m, bucketed) = m A..= A.object (map count (Map.toList bucketed))
        count (i, names)           = T.pack (show i) A..= map (map unName) names
     in A.object (("sample" A..= sample):map meth (Map.toList bucketed))

instance A.FromJSON Rep where
  parseJSON (A.Object hm) = do
    let vals     = HM.toList hm
        sample   = snd . head . filter ((== "sample") . fst) $ vals
        bucketed = filter ((/= "sample") . fst) $ vals

        convert1 :: (T.Text, A.Value) -> (Method, Map.Map Int [[Name]])
        convert1 (m, A.Object hm) = (Method m, Map.fromList
                                                 (map convert2 (HM.toList hm)))

        convert2 :: (T.Text, A.Value) -> (Int, [[Name]])
        convert2 (n, v) = (read (T.unpack n), case A.fromJSON v of
                                                A.Error err  -> error err
                                                A.Success ns -> ns)

        bucketed' = map convert1 bucketed
    sample' <- A.parseJSON sample
    pure $! force (Rep sample' (Map.fromList bucketed'))
  parseJSON v = mzero

astsOf :: ([TL.Text] -> [TL.Text]) -> [BucketUtil.Name] -> [BucketUtil.AST]
astsOf f = map convert . f . map (TL.fromStrict . BucketUtil.unName)
  where convert a = case A.eitherDecode (TE.encodeUtf8 a) of
                      Left err -> error err
                      Right x  -> x

astToId :: AST -> HT.Identifier
astToId x = HT.ID { HT.idPackage = p, HT.idModule = m , HT.idName = n }
  where [Just (A.String !p), Just (A.String !m), Just (A.String !n)] =
          map (\k -> HM.lookup k ast) ["package", "module", "name"]
        ast = getAST x

-- Streaming JSON parsing/printing

-- This type lets us avoid hard-coding IO, so that we can test some functions
-- more easily
data StreamImp m = StreamImp {
    getchar     ::                   m Char
  , getcontents ::                   m LBS.ByteString
  , info        ::         String -> m ()
  , putchar     ::           Char -> m ()
  , putstr      :: LBS.ByteString -> m ()
  }

debug :: String -> IO ()
debug msg = do shouldDebug <- lookupEnv "DEBUG"
               case shouldDebug of
                 Just "1" -> hPutStrLn stderr msg
                 _        -> pure ()

-- Specialises StreamImp to IO
io = StreamImp {
    getchar     = getChar
  , getcontents = LBS.getContents
  , info        = debug
  , putchar     = putChar
  , putstr      = LBS.putStr
  }

bucketStdio :: [Bucketer] -> ([TL.Text] -> [TL.Text]) -> IO ()
bucketStdio brs f = processSizes (astsOf f, brs)

-- We read in JSON and write out JSON with the same structure, plus some extras.
-- It's wasteful to accumulate this in memory, so instead we process one rep at
-- a time.

putErr = LBS.hPut stderr

processSizes opts      =  streamKeyVals (processSize opts)

processSize  opts size = do LBS.putStr (LBS.filter (/= ',') size)
                            putChar ':'
                            processKeyVals (processRep' opts size)

type Args = ([Name] -> [AST], [Bucketer])

processRep' :: Args -> LBS.ByteString -> LBS.ByteString -> LBS.ByteString
            -> IO ()
processRep' (astsOf, bucketers) size !key !bs =
    do putErr (LBS.concat ["Size ", size, " rep ", key, "\n"])
       LBS.putStr key
       LBS.putStr ": "
       LBS.putStr go
  where go = case A.eitherDecode bs' of
               Left _ -> case A.eitherDecode bs' of
                           Right A.Null -> A.encode A.Null
                           _            -> error (show (("Error",
                                                         "Not rep or null"),
                                                        ("Given", bs')))
               Right (Rep ns bkt) -> A.encode [A.object ["sampleNames" A..= ns],
                                               bktObj (addBuckets ns bkt)      ]

        bktObj = let meth (Method m, bktd) = m A..= A.object
                                                      (map count
                                                           (Map.toList bktd))
                     count (i, ns) = T.pack (show i) A..= map (map unName) ns
                  in A.object . map meth . Map.toList

        bs' = LBS.dropWhile notStart bs

        addBuckets smp bkt = Map.unions (bkt:map (bucket smp) bucketers)

        bucket smp = bucketCounts [1..20] (astsOf smp)

        notStart '{' = False
        notStart 'n' = False
        notStart 'N' = False
        notStart _   = True

-- Return the next non-space Char from stdin; treat : and , as whitespace
skipSpace = skipSpace' io

skipSpace' imp = do c <- getchar imp
                    if C.isSpace c || c `elem` (":," :: String)
                       then skipSpace' imp
                       else pure c

trimKey = LBS.reverse . go . LBS.reverse . go
  where go = LBS.dropWhile (/= '"')

-- Reads a '{' delimiter, then loops: reads a key and value from stdin, calls f
-- on them, then repeats; stops once the corresponding '}' gets read.
processKeyVals :: (LBS.ByteString -> LBS.ByteString -> IO ()) -> IO ()
processKeyVals f = do c <- skipSpace
                      case c of
                        '{' -> putChar '{' >> go True
                        _   -> error (show (("Expected", '{'), ("Got", c)))
  where go first = do mk <- parseOne
                      case mk of
                        Nothing -> putChar '}' -- We've hit, and consumed, the '}'
                        Just !k -> do mv <- parseOne
                                      case mv of
                                        Nothing -> error (show (("Error",
                                                                 "Key missing value"),
                                                                ("Key", k)))
                                        Just !v -> do if first
                                                         then pure ()
                                                         else putChar ','
                                                      f (trimKey k) v
                                                      go False

-- Reads a '{' delimiter, then loops: reads a key from stdin, calls f with that
-- key, then repeats (f is responsible for reading the value); stops once the
-- corresponding '}' gets read.
streamKeyVals :: (LBS.ByteString -> IO ()) -> IO ()
streamKeyVals f = streamKeyVals' io f

streamKeyVals' :: Monad m => StreamImp m -> (LBS.ByteString -> m ()) -> m ()
streamKeyVals' imp f = do c <- skipSpace' imp
                          case c of
                            '{' -> putchar imp '{' >> go True
                            _   -> do rest <- getcontents imp
                                      error (show (("Wanted", '{'             ),
                                                   ("Got"   , c               ),
                                                   ("Rest"  , LBS.take 50 rest)))
  where go first = do mk <- parseOne' imp
                      case mk of
                        Nothing -> putchar imp '}' -- We've hit, and consumed, the '}'
                        Just !k -> do if first
                                         then pure ()
                                         else putchar imp ','
                                      f (trimKey k)
                                      go False

-- Parse a JSON container from stdin (a string, array, object or null), return
-- the ByteString it spans. Returns Nothing if we hit an unexpected close
-- delimiter; this lets us consume container contents by running parseOne over
-- and over until we get Nothing.

data Container = CObject | CArray | CString

data ParseState = PSTop | PSInside !Container !ParseState

parseOne :: IO (Maybe LBS.ByteString)
parseOne = parseOne' io

parseOne' :: Monad m => StreamImp m -> m (Maybe LBS.ByteString)
parseOne' imp = getchar imp >>= go LBS.empty PSTop
  where snocTo   bs c   = pure $! (Just $! LBS.snoc bs c)

        readSnoc bs c s = getchar imp >>= go (LBS.snoc bs c) s

        go !bs s c = case (s, c) of
          -- Pop the state when we see our closing delimiter; return if finished
          (PSInside CObject PSTop, '}') -> snocTo   bs '}'
          (PSInside CObject s'   , '}') -> readSnoc bs '}' s'
          (PSInside CArray  PSTop, ']') -> snocTo   bs ']'
          (PSInside CArray  s'   , ']') -> readSnoc bs ']' s'
          (PSInside CString PSTop, '"') -> snocTo   bs '"'
          (PSInside CString s'   , '"') -> readSnoc bs '"' s'

          -- Fail if we've hit the end of a container we're not in
          (PSTop, '}') -> pure Nothing
          (PSTop, ']') -> pure Nothing

          -- Special case to handle escaped quotes and escaped escapes
          (PSInside CString s', '\\') -> do c' <- getchar imp
                                            getchar imp >>=
                                              go (LBS.snoc (LBS.snoc bs c) c') s

          -- Skip everything else if we're in a string
          (PSInside CString _, _) -> getchar imp >>= go (LBS.snoc bs c) s

          -- Always skip whitespace
          _ | C.isSpace c -> getchar imp >>= go (LBS.snoc bs c) s

          -- Skip over ':' and ',' as if they're whitespace
          (_, ':') -> readSnoc bs c s
          (_, ',') -> readSnoc bs c s

          -- Accept null as a value
          (_, 'n') -> do ['u', 'l', 'l'] <- replicateM 3 (getchar imp)
                         pure $! (Just $! L.foldl' LBS.snoc bs
                                                   ("null" :: String))

          -- Skip over numbers
          _ | C.isDigit c -> readSnoc bs c s

          -- Skip over true
          (_, 't') -> do cs <- replicateM 3 (getchar imp)
                         getchar imp >>= go (L.foldl' LBS.snoc bs cs) s

          -- Skip over false
          (_, 'f') -> do cs <- replicateM 4 (getchar imp)
                         getchar imp >>= go (L.foldl' LBS.snoc bs cs) s

          -- Match an opening delimiter
          (_, '{') -> readSnoc bs '{' (PSInside CObject s)
          (_, '[') -> readSnoc bs '[' (PSInside CArray  s)
          (_, '"') -> readSnoc bs '"' (PSInside CString s)
