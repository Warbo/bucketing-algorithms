{-# LANGUAGE OverloadedStrings #-}
module AstsHelpers where
import Control.Monad (mzero)
import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Map.Lazy              as Map
import qualified Data.Text.Lazy             as T
import qualified Data.Text.Lazy.Encoding    as TE

-- Data types and JSON parsers/printers

newtype Name = Name { unName :: T.Text } deriving (Eq, Ord)

instance Aeson.FromJSON Name where
  parseJSON x = Name <$> Aeson.parseJSON x

newtype AST = AST { unAST :: T.Text }

instance Aeson.ToJSON AST where
  toJSON (AST x) = Aeson.toJSON x

newtype NamedAST = NAST { unNamed :: (Name, AST) }

type ASTMap = Map.Map Name AST

instance Aeson.FromJSON NamedAST where
  parseJSON j = case j of
      Aeson.Object o -> do n <- o Aeson..: "name"
                           pure (NAST (n, objectToAST o))
      _              -> mzero
    where objectToAST = AST . TE.decodeUtf8 . Aeson.encode

-- Parses a JSON array of ASTs from the given ByteString and creates
-- a map from names to ASTs
mkASTMap :: BS.ByteString -> ASTMap
mkASTMap encoded = Map.fromList (map unNamed asts)
  where asts :: [NamedAST]
        asts = case Aeson.eitherDecode encoded of
                    Left err -> error err
                    Right xs -> xs
