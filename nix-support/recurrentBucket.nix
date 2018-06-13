# Commands which split their input into various "buckets", e.g. based on
# clustering. We don't do any exploration or reduction, we just look at the
# resulting buckets.
{ bash, bc, bucketCheck, cluster, format, haskellPackages, jq, mkBin,
  runCommand, runWeka, stdenv, withDeps, wrap, writeScript }:

with rec {
  haskellVersion = runCommand "recurrent-bucket"
    {
      buildInputs = [
        (haskellPackages.ghcWithPackages (hs: [
          hs.aeson
          hs.ML4HSFE
          hs.process
          hs.process-extras
          hs.unordered-containers
          hs.vector
        ]))
      ];
      main = writeScript "recurrent-bucket-main.hs" ''
        {-# LANGUAGE OverloadedStrings #-}
        module Main where
        import qualified Data.Aeson                 as A
        import qualified Data.ByteString.Char8      as BS
        import qualified Data.ByteString.Lazy.Char8 as LBS
        import qualified Data.Char                  as C
        import qualified Data.HashMap.Strict        as H
        import qualified Data.Vector                as V
        import           ML4HSFE.Loop
        import           ML4HSFE.Outer
        import           System.Environment
        import           System.Exit
        import           System.IO
        import qualified System.Process             as P
        import qualified System.Process.ByteString  as PB

        cluster s Nothing      height        = cluster s (Just "30") height
        cluster s width        Nothing       = cluster s width       (Just "30")
        cluster s (Just width) (Just height) = do
          asts <- clusterLoop (handleString (read width)
                                            (read height)
                                            (LBS.fromStrict s))
          LBS.putStr (processAsts asts)

        bsToAsts :: BS.ByteString -> A.Value
        bsToAsts s = case A.eitherDecode (LBS.fromStrict s) of
                       Left  e -> error ("Failed to read ASTs " ++ e)
                       Right x -> x

        processAsts = A.encode . H.elems . V.foldl' acc H.empty
          where acc h v = case v of
                            A.Object o -> ins h o
                            _          -> error ("Expected obj got " ++ show v)

                ins h o = let A.Number n = o H.! "cluster"
                           in case (o H.! "quickspecable", o H.! "type") of
                                (_,           A.Null) -> h
                                (A.Bool True, _     ) ->
                                  H.insertWith (++)
                                               n
                                               [A.Object
                                                 (H.delete "tocluster"
                                                   (H.delete "features" o))]
                                               h

        main = do i <- BS.getContents
                  if BS.all C.isSpace i
                     then BS.putStr "[]"
                     else do width  <- lookupEnv "WIDTH"
                             height <- lookupEnv "HEIGHT"
                             cluster i width height
      '';
    }
    ''
      cp "$main" Main.hs
      ghc --make -o Main Main.hs
      mv Main "$out"
    '';

  cmd = mkBin {
    name  = "recurrentBucket";
    paths = [ bash jq runWeka ];
    file  = haskellVersion;
  };

  check = bucketCheck {
    inherit cmd;
    name = "recurrent";
    go   = "recurrentBucket";
  };
};

withDeps [ check ] cmd
