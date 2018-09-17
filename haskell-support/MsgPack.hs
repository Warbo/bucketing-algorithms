{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module MsgPack where

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

-- | Actions which may write to stdout
type StdOut = IO

-- | Consumes an 'a' from the start of a ByteString, writes a 'b' to stdout and
--   returns the remainder of the ByteString. Note that 'a' and 'b' are phantom.
data Consumer a b = Consumer {
    consume :: BS.ByteString -> StdOut BS.ByteString
  }

data MsgPackMap k v

-- | Key/value pair for a (hash)map. These are encoded one after the other,
--   without an extra header at the start (i.e. they're not a 2-element array)
data MsgPackKeyVal k v

instance IsString Object where
  fromString = ObjectStr . T.pack

err :: Show a => a -> b
err = error . show

putOut :: BP.Put -> StdOut ()
putOut = BS.putStr . BP.runPut

-- Parses the header of a MessagePack map (object). Returns the length. Leaves
-- the actual keys and values to be parsed separately.
getMapHeader :: String -> BG.Get Int
getMapHeader label = do
  t <- BG.getWord8
  case t of
    0xDE -> fromIntegral <$> BG.getWord16be
    0xDF -> fromIntegral <$> BG.getWord32be
    _    -> if t .&. 0xF0 == 0x80
               then return $ fromIntegral $ t .&. 0x0F
               else error ("Couldn't parse map header for " ++ label)

writeMapHeader :: Int -> StdOut ()
writeMapHeader n = putOut (BP.putWord8 0xDF >>
                           BP.putWord32be (fromIntegral n))

data RepType = RepTypeNull | RepTypeData

-- | Parse the header of a rep. This tells us if it's null (i.e. a duplicate) or
--   whether there's actual data to be had.
getRepHeader :: String -> BG.Get RepType
getRepHeader label = do
  t <- BG.getWord8
  case t of
    0xC0 -> pure RepTypeNull

    -- There are 3 ways to represent arrays; we skip the length (it's always 2)
    0x92 ->                   pure RepTypeData  -- Hard-coded length of 2
    0xDC -> BG.getWord16be >> pure RepTypeData  -- Skip following length
    0xDD -> BG.getWord32be >> pure RepTypeData  -- Skip following length

    _ -> let arr = t .&. 0xF0 == 0x90
          in error (show (("error"    , "Invalid rep header"),
                          ("label"    , label               ),
                          ("byte"     , t                   ),
                          ("is array?", arr                 ),
                          ("length"   , if arr
                                           then show (fromIntegral (t .&. 0x0F))
                                           else "N/A")))

-- | Maps (as in fmap) a consumer of (key,value) pairs over the contents of a
--   MessagePack map (as in hashmap). The label is used for error messages.
mapMap :: String -> Consumer (MsgPackKeyVal kIn vIn) (MsgPackKeyVal kOut vOut)
                 -> Consumer (MsgPackMap    kIn vIn) (MsgPackMap    kOut vOut)
mapMap label f = Consumer run
  where run !bs = let (n, bs') = parseNext (label ++ " length")
                                           (getMapHeader label)
                                           bs
                   in writeMapHeader n >> go (n, bs')

        go (!n, !bs') = if n == 0
                           then pure bs'
                           else do remaining <- consume f bs'
                                   go (n-1, remaining)

parseNext :: String -> BG.Get a -> BS.ByteString -> (a, BS.ByteString)
parseNext label p = go (BG.runGetIncremental p)
  where -- Parsing loop: as long as it's Partial, keep passing in Chunks from
        -- our (lazy) ByteString. When Done, stick any leftover (strict)
        -- ByteString at the front of the remaining Chunks.
        go dec bs = case dec of
          BG.Fail _ pos msg -> error (unwords ["Failed to parse", label ++ ",",
                                               "Message:"       , msg         ])

          -- Not enough input yet; pass in another Chunk and recurse.
          BG.Partial _ -> case bs of
                            BS.Empty -> error ("EOF parsing " ++ label)
                            BS.Chunk c cs -> go (dec `BG.pushChunk` c) cs

          -- Done. Stick the leftover ByteString in front of the rest.
          BG.Done !bs' _ !x -> (x, BS.Chunk bs' bs)

writeMP :: MP.Object -> StdOut ()
writeMP = BS.putStr . MP.pack

unArray (ObjectArray a) = a
unArray x               = error ("Expected array, got " ++ show x)

-- | Turns a consumer of values into a consumer of key/value pairs, where the
--   key in simply passed along unchanged.
dumpKey :: String -> Consumer vIn vOut -> Consumer (MsgPackKeyVal key vIn )
                                                   (MsgPackKeyVal key vOut)
dumpKey label c = Consumer go
  where go bs = let (key, bs') = parseNext ("dumpKey " ++ label) MP.getObject bs
                 in writeMP key >> consume c bs'


msgPackMain :: Consumer a b -> StdOut ()
msgPackMain c = BS.getContents >>= consume c >> pure ()
