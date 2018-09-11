# Takes a Haskell module implementing a bucketing algorithm and generates a
# standalone binary for running that algorithm.
{ attrsToDirs', runCommand, writeScript }:

{ buildInputs, files ? {}, mod, name }: runCommand name
  {
    inherit buildInputs;
    files = attrsToDirs' "${name}-files" files;
    main  = writeScript "${name}-main.hs" ''
      {-# LANGUAGE OverloadedStrings #-}
      module Main where
      import           BucketUtil
      import           Control.Applicative        ((<|>))
      import qualified Data.Aeson                 as A
      import qualified Data.ByteString.Char8      as BS
      import qualified Data.ByteString.Lazy.Char8 as LBS
      import qualified Data.Char                  as C
      import           Data.Maybe                 (fromJust)
      import           System.Environment         (lookupEnv)
      import           System.IO.Unsafe           (unsafePerformIO)
      import qualified ${mod}

      main = do i <- LBS.getContents
                if LBS.all C.isSpace i
                   then LBS.putStr "[]"
                   else LBS.putStr . A.encode $ case A.eitherDecode i of
                          Left  err  -> error err
                          Right asts -> ${mod}.bucket (clusters asts) asts
    '';
  }
  ''
    cp -v "$files"/* ./
    cp -v "$main" Main.hs
    ghc --make -O2 -o Main Main.hs
    mv Main "$out"
  ''
