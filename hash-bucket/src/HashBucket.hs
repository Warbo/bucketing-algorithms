module Main where

import qualified Crypto.Hash           as Hash
import qualified Data.Aeson            as Aeson
import qualified Data.Aeson.Parser     as Parser
import qualified Data.ByteArray        as BA
import qualified Data.ByteString.Char8 as BS
import qualified Data.Maybe            as M
import qualified System.Environment    as Env

wrapUp xs@(Aeson.Object o:_) = Aeson.Array xs
wrapUp [Aeson.Array a]       = a
wrapUp x                     = error ("Can't wrap up " ++ show x)

clusterCount :: [BS.ByteString] -> IO Int
clusterCount xs = ceiling <$> chosen
  where chosen :: IO Float
        chosen = maybe (sqrt count) fromSize <$> Env.lookupEnv "CLUSTER_SIZE"

        count :: Float
        count = fromInteger (length xs)

        fromSize :: String -> Float
        fromSize size = count / read size

onlyName :: Aeson.Parser BS.ByteString
onlyName = arrayOf ("name" .: string)

allNames :: Parser.Parser [BS.ByteString]
allNames = concat <$> many names

names :: Parser.Parser [BS.ByteString]
names = do toplevel <- json
           case toplevel of
             Object _ -> name topLevel
             Array  a -> map name (toList a)
  where name (Object o) = fromJust (lookup "name" o)

hash :: BS.ByteString -> BS.ByteString
hash = BA.convert . Hash.hashWith SHA256

main = do input <- getContents
          let names = Parser.decodeWith allNames input
          clusters <- clusterCount names
          print ("Got clusters", clusters)
