{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell       #-}
module AstsOf where
import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Map.Lazy              as Map
import qualified Data.Text.Lazy             as T
import qualified Data.Text.Lazy.Encoding    as TE
import AstsHelpers
import Instances.TH.Lift  -- So we can 'lift' a Map
import Language.Haskell.TH.Syntax (lift, runIO)

-- Runs mkASTMap on a known source of ASTs
astMap :: ASTMap
astMap = Map.fromList (map (\(n, a) -> (Name n, AST a))
  $(do let f = "REPLACEME"
       bs <- runIO (BS.readFile f)
       let m = mkASTMap bs
           l = Map.toList m
       lift (map (\(n, a) -> (unName n, unAST a)) l)))

-- Parse, lookup, print

--astsOf :: [Name] -> [AST]
astsOf = map get
  where get n        = Map.findWithDefault (err n) n astMap
        err (Name n) = error ("No AST for " ++ show n)

astsOf' = map unAST . astsOf . map Name

namesToAsts :: BS.ByteString -> BS.ByteString
namesToAsts s = case Aeson.eitherDecode s of
  Left err -> error err
  Right ns -> render (astsOf ns)

render = TE.encodeUtf8       .
         (`T.snoc` ']')      .
         T.cons '['          .
         T.intercalate ",\n" .
         map unAST

-- Reads JSON array of names from stdin, prints JSON array of ASTs
main = BS.interact namesToAsts
