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
import           System.IO                  (stderr)
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

bucketSizes :: [Int] -> [AST] -> Bucketer -> Bucketed
bucketSizes sizes asts (method, bucket) = force (Map.singleton method results)
  where results = Map.fromList (map go sizes)
        go size = let buckets = map (map (mkName . HT.idName)) (bucket size asts)
                   in size `deepseq` buckets `deepseq` (size, buckets)

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
    let meth (Method m, bucketed) = m A..= A.object (map size (Map.toList bucketed))
        size (i, names)           = T.pack (show i) A..= map (map unName) names
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

bucketStdio :: [Bucketer] -> ([TL.Text] -> [TL.Text]) -> IO ()
bucketStdio brs f = runHandler (processSizes (astsOf f, brs))

-- We read in JSON and write out JSON with the same structure, plus some extras.
-- It's wasteful to accumulate this in memory, so instead we process one rep at
-- a time. Assuming there are nested JSON objects on stdin, the 'Handler' type
-- tells us how to handle each nested level: Blocking is a simple function which
-- is given ByteStrings of the key and value, and should write its own to
-- stdout. Streaming applies a Handler to each key.

data Handler = Blocking  (LBS.ByteString -> LBS.ByteString -> IO ())
             | Streaming (LBS.ByteString -> Handler)

putErr = LBS.hPut stderr

processSizes opts      = Streaming (processSize opts)

processSize  opts size = Blocking (processRep'  opts size)

--processRep   opts rep  = Blocking  (processRep' opts)

processRep' :: (_, _) -> LBS.ByteString -> LBS.ByteString -> LBS.ByteString
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
               Right (Rep smp bkt) -> A.encode (Rep smp (addBuckets smp bkt))

        bs' = LBS.dropWhile notStart bs

        addBuckets smp bkt = Map.unions (bkt:map (bucket smp) bucketers)

        bucket smp = bucketSizes [1..20] (astsOf smp)

        notStart '{' = False
        notStart 'n' = False
        notStart 'N' = False
        notStart _   = True

runHandler h = case h of
    Blocking  f -> processKeyVals f
    Streaming f ->  streamKeyVals f

-- Return the next non-space Char from stdin; treat : and , as whitespace
skipSpace = do c <- getChar
               if C.isSpace c || c `elem` (":," :: String)
                  then skipSpace
                  else pure c

-- Reads a '{' delimiter, then loops: reads a key and value from stdin, calls f
-- on them, then repeats; stops once the corresponding '}' gets read.
processKeyVals :: (LBS.ByteString -> LBS.ByteString -> IO ()) -> IO ()
processKeyVals f = do c <- skipSpace
                      case c of
                        '{' -> go
                        _   -> error (show (("Expected", '{'), ("Got", c)))
  where go = do mk <- parseOne
                case mk of
                  Nothing -> pure () -- We've hit, and consumed, the '}'
                  Just !k -> do mv <- parseOne
                                case mv of
                                  Nothing -> error (show (("Error",
                                                           "Key missing value"),
                                                          ("Key", k)))
                                  Just !v -> f k v >> go

-- Reads a '{' delimiter, then loops: reads a key from stdin, calls f with that
-- key, then repeats (f is responsible for reading the value); stops once the
-- corresponding '}' gets read.
streamKeyVals :: (LBS.ByteString -> Handler) -> IO ()
streamKeyVals f = do c <- skipSpace
                     case c of
                       '{' -> go
                       _   -> error (show (("Wanted", '{'), ("Got", c)))
  where go = do mk <- parseOne
                case mk of
                  Nothing -> pure () -- We've hit, and consumed, the '}'
                  Just !k -> runHandler (f k) >> go

-- Parse a JSON container from stdin (a string, array, object or null), return
-- the ByteString it spans. Returns Nothing if we hit an unexpected close
-- delimiter; this lets us consume container contents by running parseOne over
-- and over until we get Nothing.

data Container = CObject | CArray | CString

data ParseState = PSTop | PSInside !Container !ParseState

parseOne :: IO (Maybe LBS.ByteString)
parseOne = getChar >>= go LBS.empty PSTop
  where go !bs s c = case (s, c) of
          -- Pop the state when we see our closing delimiter; return if finished
          (PSInside CObject PSTop, '}') -> pure $! (Just $! LBS.snoc bs '}')
          (PSInside CObject s'   , '}') -> getChar >>= go (LBS.snoc bs '}') s'
          (PSInside CArray  PSTop, ']') -> pure $! (Just $! LBS.snoc bs ']')
          (PSInside CArray  s'   , ']') -> getChar >>= go (LBS.snoc bs ']') s'
          (PSInside CString PSTop, '"') -> pure $! (Just $! LBS.snoc bs '"')
          (PSInside CString s'   , '"') -> getChar >>= go (LBS.snoc bs '"') s'

          -- Fail if we've hit the end of a container we're not in
          (PSTop, '}') -> pure Nothing
          (PSTop, ']') -> pure Nothing

          -- Special case to handle escaped quotes and escaped escapes
          (PSInside CString s', '\\') -> do c' <- getChar
                                            getChar >>= go (LBS.snoc
                                                             (LBS.snoc bs c) c')
                                                           s

          -- Skip everything else if we're in a string
          (PSInside CString _, _) -> getChar >>= go (LBS.snoc bs c) s

          -- Always skip whitespace
          _ | C.isSpace c -> getChar >>= go (LBS.snoc bs c) s

          -- Skip over ':' and ',' as if they're whitespace
          (_, ':') -> getChar >>= go (LBS.snoc bs c) s
          (_, ',') -> getChar >>= go (LBS.snoc bs c) s

          -- Accept null as a value
          (_, 'n') -> do ['u', 'l', 'l'] <- replicateM 3 getChar
                         pure $! (Just $! L.foldl' LBS.snoc bs
                                                   ("null" :: String))

          -- Skip over numbers
          _ | C.isDigit c -> getChar >>= go (LBS.snoc bs c) s

          -- Skip over true
          (_, 't') -> do cs <- replicateM 3 getChar
                         getChar >>= go (L.foldl' LBS.snoc bs cs) s

          -- Skip over false
          (_, 'f') -> do cs <- replicateM 4 getChar
                         getChar >>= go (L.foldl' LBS.snoc bs cs) s

          -- Match an opening delimiter
          (_, '{') -> getChar >>= go (LBS.snoc bs '{') (PSInside CObject s)
          (_, '[') -> getChar >>= go (LBS.snoc bs '[') (PSInside CArray  s)
          (_, '"') -> getChar >>= go (LBS.snoc bs '"') (PSInside CString s)
