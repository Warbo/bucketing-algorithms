{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
module BucketUtil where

import           Control.Applicative        ((<|>))
import           Control.DeepSeq            (NFData, deepseq, force, rnf, ($!!))
import           Control.Monad              (mzero)
import           Control.Monad.State.Strict (State, get, put, replicateM_,
                                             runState)
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Char                  as C
import qualified Data.HashMap.Strict        as HM
import qualified Data.Interned              as I
import qualified Data.Interned.Text         as IT
import           Data.IORef
import qualified Data.List                  as L
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (fromJust)
import qualified Data.String                as S
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Encoding    as TE
import           Debug.Trace                (trace)
import           GHC.Generics               (Generic)
import           GHC.Int                    (Int64)
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
  -- Consume and return a single Char from the input
    getchar     ::                   m Char

  -- Consume and return the entire input
  , getcontents ::                   m LBS.ByteString

  -- Consume Chars until the given condition is False. Allowing state s makes
  -- context-free parsing easier. Returns what we consumed and the final state.
  , getuntil :: forall s. (s -> Char -> (s, Bool)) -> s -> m (s, LBS.ByteString)

  -- Log debugging info
  , info        ::         String -> m ()

  -- Write a Char to the output
  , putchar     ::           Char -> m ()

  -- Write a whole string to the output
  , putstr      :: LBS.ByteString -> m ()
  }

debug :: String -> IO ()
debug msg = do shouldDebug <- lookupEnv "DEBUG"
               case shouldDebug of
                 Just "1" -> hPutStrLn stderr msg
                 _        -> pure ()

-- | Buffered IO implementation of our streaming interface. In particular, this
--   version reads the whole of stdin into a buffer, then acts on this buffer
--   from then on. Using a lazy bytestring for our buffer ensures it will be
--   read on-demand, in reasonably-sized chunks rather than all at once or one
--   character at a time.
--
--  Note that we must only create one implementation and pass it around. If we
--  try using two it will cause 'getContents' to be called twice, which will
--  complain that stdin has been closed (by the first call). That's why there's
--  an 'IO' wrapper around this.
buf :: IO (StreamImp IO)
buf = do -- Read all stdin (lazily) into a local buffer
         b <- LBS.getContents >>= newIORef
         pure $ StreamImp {
             getchar     = apply b getchar'
           , getcontents = apply b (\s -> (LBS.empty, s))
           , getuntil    = \f s -> apply b (getuntil' f s)

           -- The following don't use stdin, so we can defer to io
           , info        = LBS.hPut stderr . LBS.pack
           , putchar     = putChar
           , putstr      = LBS.putStr
           }
  where -- Update the buffer (strictly) and return a value
        apply :: IORef LBS.ByteString
              -> (LBS.ByteString -> (LBS.ByteString, a))
              -> IO a
        apply b f = atomicModifyIORef' b f

        getchar' bs = case LBS.uncons bs of
                        Just (c, bs') -> (bs', c)
                        Nothing       -> error "Reached EOF in getchar"

        -- The given function is folded over the given ByteString, accumulating
        -- a state until it decides to stop. We count the number of steps taken.
        -- The final state is included in our output, but doesn't determine the
        -- ByteStrings we return. Those are a suffix and prefix of the given
        -- ByteString, respectively; split using the counter we made. This
        -- counting and splitting is more efficient than building up ByteStrings
        -- one Char at a time.
        getuntil' :: forall state
                   . (state -> Char -> (state, Bool))
                  -> state
                  -> LBS.ByteString
                  -> (LBS.ByteString, (state, LBS.ByteString))
        getuntil' f initial str =
          let -- Our loop; calls 'f' on each Char, passing along the state and
              -- counting the number of steps taken.
              countUntil :: Int64 -> state -> LBS.ByteString -> (Int64, state)
              countUntil !n !s !bs = case LBS.uncons bs of
                Nothing         -> error (show (("error", "Ran out of input" )
                                               ,("where", "Buffered getuntil")
                                               ,("n"    , n                  )
                                               ))
                Just (!c, !bs') -> case f s c of
                  (!s', True ) -> (n+1, s')
                  (!s', False) -> countUntil (n+1) s' bs'

           -- Start the loop, pass along the final state and use the counter to
           -- split the ByteString. Effect is like calling 'getchar' 'n' times.
           in case countUntil 0 initial str of
                (!n, !final) -> case LBS.splitAt n str of
                                  (!pre, !suf) -> (suf, (final, pre))

-- We read in JSON and write out JSON with the same structure, plus some extras.
-- It's wasteful to accumulate this in memory, so instead we process one rep at
-- a time.

type Args = ([Name] -> [AST], [Bucketer])

bucketStdio :: forall m
             . Monad m
            => StreamImp m
            -> [Bucketer]
            -> ([TL.Text] -> [TL.Text])
            -> m ()
bucketStdio imp brs f = processSizes (astsOf f, brs)
  where processSizes opts      =  streamKeyVals imp (processSize opts)

        processSize  opts size = do
          putstr imp (LBS.filter (/= ',') size)
          putchar imp ':'
          processKeyVals imp (processRep imp opts size)

processRep :: Monad m
           => StreamImp m
           -> Args
           -> LBS.ByteString
           -> LBS.ByteString
           -> LBS.ByteString
           -> m ()
processRep imp (astsOf, bucketers) size !key !bs = do
    mapM_ (info imp) ["Size ", LBS.unpack size, " rep ", LBS.unpack key, "\n"]
    putstr imp key
    putstr imp ": "
    putstr imp go

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

-- | Return the next non-space Char from stdin; treat : and , as whitespace
skipSpace imp = do c <- getchar imp
                   if C.isSpace c || c `elem` (":," :: String)
                      then skipSpace imp
                      else pure c

trimKey = LBS.reverse . go . LBS.reverse . go
  where go = LBS.dropWhile (/= '"')

-- Reads a '{' delimiter, then loops: reads a key and value from stdin, calls f
-- on them, then repeats; stops once the corresponding '}' gets read.
processKeyVals :: Monad m
               => StreamImp m
               -> (LBS.ByteString -> LBS.ByteString -> m ())
               -> m ()
processKeyVals imp f = do c <- skipSpace imp
                          case c of
                            '{' -> putchar imp '{' >> go True
                            _   -> error (show (("Expected", '{'), ("Got", c)))
  where go first = do mk <- parseOne imp
                      case mk of
                        Nothing -> putchar imp '}' -- Found and consumed the '}'
                        Just !k -> do mv <- parseOne imp
                                      case mv of
                                        Nothing -> error
                                          (show (("Error", "Key missing value")
                                                ,("Key"  , k)
                                                ))
                                        Just !v -> do if first
                                                         then pure ()
                                                         else putchar imp ','
                                                      f (trimKey k) v
                                                      go False

-- Reads a '{' delimiter, then loops: reads a key from stdin, calls f with that
-- key, then repeats (f is responsible for reading the value); stops once the
-- corresponding '}' gets read.
streamKeyVals :: Monad m => StreamImp m -> (LBS.ByteString -> m ()) -> m ()
streamKeyVals imp f = do c <- skipSpace imp
                         case c of
                           '{' -> putchar imp '{' >> go True
                           _   -> do rest <- getcontents imp
                                     error (show (("Wanted", '{'             ),
                                                  ("Got"   , c               ),
                                                  ("Rest"  , LBS.take 50 rest)))
  where go first = do mk <- parseOne imp
                      case mk of
                        Nothing -> putchar imp '}' -- Hit & consume the '}'
                        Just !k -> do if first
                                         then pure ()
                                         else putchar imp ','
                                      f (trimKey k)
                                      go False

-- Parse a JSON container from stdin (a string, array, object or null), return
-- the ByteString it spans. Returns Nothing if we hit an unexpected close
-- delimiter; this lets us consume container contents by running parseOne over
-- and over until we get Nothing.

data Container = CObject | CArray | CString | CEscaped | CKeyword Int
  deriving (Show)

data ParseState = PSTop | PSInside !Container !ParseState deriving (Show)

parseOne :: Monad m => StreamImp m -> m (Maybe LBS.ByteString)
parseOne imp = do result <- getuntil imp go (Just PSTop)
                  return $ case result of
                    (Nothing, _) -> Nothing
                    (Just _ , s) -> Just s
  where err = (Nothing, True)  -- Short hand for stopping with an error

        -- We assume the state is Just, since we stop when switching to Nothing
        go :: Maybe ParseState -> Char -> (Maybe ParseState, Bool)
        go (Just s) c = case (s, c) of
          -- Pop the state when we see our closing delimiter; return if finished
          (PSInside CObject PSTop, '}') -> (Just undefined, True )
          (PSInside CObject s'   , '}') -> (Just s'       , False)
          (PSInside CArray  PSTop, ']') -> (Just undefined, True )
          (PSInside CArray  s'   , ']') -> (Just s'       , False)
          (PSInside CString PSTop, '"') -> (Just undefined, True )
          (PSInside CString s'   , '"') -> (Just s'       , False)

          -- Fail if we've hit the end of a container we're not in
          (PSTop, '}') -> (Nothing, True)
          (PSTop, ']') -> (Nothing, True)

          -- If we hit \ in a string, switch to the escaped state
          (PSInside CString _, '\\') -> (Just (PSInside CEscaped s), False)

          -- Skip everything else if we're in a string
          (PSInside CString _, _) -> (Just s, False)

          -- Escape quotes and backslashes in strings
          (PSInside CEscaped s'@(PSInside CString _), '"' ) -> (Just s', False)
          (PSInside CEscaped s'@(PSInside CString _), '\\') -> (Just s', False)

          -- Don't escape anything else
          (PSInside CEscaped (PSInside CString _), _) -> err
          (PSInside CEscaped _                   , _) -> err

          -- Always skip whitespace
          _ | C.isSpace c -> (Just s, False)

          -- Skip over ':' and ',' as if they're whitespace
          (_, ':') -> (Just s, False)
          (_, ',') -> (Just s, False)

          -- Pop the state when we reach the end of a keyword
          (PSInside (CKeyword 0) PSTop, _) -> (Just undefined, True )
          (PSInside (CKeyword 0) s'   , _) -> (Just s'       , False)

          -- Step through a keyword if we've not reached its end yet
          (PSInside (CKeyword n) s', c) -> (Just $ PSInside (CKeyword (n-1)) s',
                                            False)

          -- Prepare to step through null, true and false
          (_, 'n') -> (Just $ PSInside (CKeyword 2) s, False)
          (_, 't') -> (Just $ PSInside (CKeyword 2) s, False)
          (_, 'f') -> (Just $ PSInside (CKeyword 3) s, False)

          -- Skip over numbers
          _ | C.isDigit c -> (Just s, False)

          -- Match an opening delimiter
          (_, '{') -> (Just $ PSInside CObject s, False)
          (_, '[') -> (Just $ PSInside CArray  s, False)
          (_, '"') -> (Just $ PSInside CString s, False)

          _ -> error (show (("error", "Unexpected Char for state")
                           ,("char" , c)
                           ,("state", s)
                           ))
