# Command which reads function names from stdin and writes annotated ASTs to
# stdout. We use Template Haskell to build a name->AST mapping ahead of time,
# so lookups don't get slowed down by disk access, parsing, etc.
{ attrsToDirs', fail, haskellPackages, jq, runCommand, tebenchmark, testData,
  withDeps, writeScript }:

with rec {
  modules = {
    "AstsHelpers.hs" = writeScript "AstsHelpers.hs" ''
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
    '';
    "AstsOf.hs" = writeScript "AstsOf.hs" ''
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
        $(do let f = "${testData.tip-benchmark.asts}"
             bs <- runIO (BS.readFile f)
             let m = mkASTMap bs
                 l = Map.toList m
             lift (map (\(n, a) -> (unName n, unAST a)) l)))

      -- Parse, lookup, print

      --astsOf :: [Name] -> [AST]
      astsOf = map get
        where get n        = Map.findWithDefault (err n) n astMap
              err (Name n) = error ("No AST for " ++ show n)

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
    '';
  };

  script = runCommand "astsOf"
    {
      modules     = attrsToDirs' "AstsOfModules" modules;
      buildInputs = [
        (haskellPackages.ghcWithPackages (h: [
          h.aeson h.bytestring h.containers h.text h.th-lift-instances
        ]))
      ];
      main = writeScript "astsof-main.hs" ''
        module Main where
        import qualified AstsOf
        main = AstsOf.main
      '';
    }
    ''
      cp -v "$modules"/*.hs ./
      cp "$main" Main.hs
      ghc --make Main.hs -o "$out"
    '';

  test = runCommand "test-astsOf"
    {
      inherit script;
      buildInputs = [ fail jq tebenchmark.tools ];
    }
    ''
      S=$(choose_sample 5 10)                    || fail "Didn't sample"
      I=$(echo "$S" | jq -R '.' | jq -s '.')     || fail "Didn't wrap"
      O=$(echo "$I" | "$script")                 || fail "Didn't get ASTs"
      echo "$O" | jq -e 'type | . == "array"'    || fail "$O\nNot object"
      echo "$O" | jq -e 'length | . == 5'        || fail "$O\nNot 5 ASTs"
      echo "$O" | jq --argjson i "$I" \
                     'map(.name) | sort | . == ($i | sort)' ||
        fail "Input:\n$I\n\nOutput:\n$O\n\nMismatching names"
      mkdir "$out"
    '';
};
{
  astsOfModules = modules;
  astsOfScript  = withDeps [ test ] script;
}
