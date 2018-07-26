# Commands which split their input into various "buckets", e.g. based on
# clustering. We don't do any exploration or reduction, we just look at the
# resulting buckets.
{ bash, bucketCheck, ghcWithML4HSFE, mkBin, runCommand, withDeps, wrap,
  writeScript }:

with rec {
  haskellVersion = runCommand "recurrent-bucket"
    {
      buildInputs = [ ghcWithML4HSFE ];
      main        = writeScript "recurrent-bucket-main.hs" ''
        {-# LANGUAGE OverloadedStrings #-}
        module Main where
        import           Control.Applicative        ((<|>))
        import qualified Data.Aeson                 as A
        import qualified Data.ByteString.Char8      as BS
        import qualified Data.ByteString.Lazy.Char8 as LBS
        import qualified Data.Char                  as C
        import qualified Data.HashMap.Strict        as H
        import           Data.Maybe                 (fromJust)
        import qualified Data.Vector                as V
        import           ML4HSFE.Loop
        import           ML4HSFE.Outer
        import           System.Environment
        import           System.IO

        cluster s Nothing      height        = cluster s (Just "30") height
        cluster s width        Nothing       = cluster s width       (Just "30")
        cluster s (Just width) (Just height) = do
          c <- getClusters s
          let asts = I.runIdentity
                       (clusterLoop (pureKmeans (Just c))
                                    (handleString (read width)
                                                  (read height)
                                                  (LBS.fromStrict s)))
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

        getClusters :: BS.ByteString -> IO Int
        getClusters s = do
          c  <- lookupEnv "CLUSTERS"
          cs <- lookupEnv "CLUSTER_SIZE"
          pure (clusters s c cs)

        clusters :: BS.ByteString -> Maybe String -> Maybe String -> Int
        clusters s c cs = fromJust (fromSize <|> fromEnv <|> Just fromIn)
          where fromSize = case cs of
                             Nothing -> Nothing
                             Just s  -> let size = fromIntegral (read s :: Int)
                                            len  = fromIntegral inCount :: Float
                                         in Just (ceil (len / size))
                fromEnv = fmap read c
                fromIn  = ceil (sqrt (fromIntegral inCount))
                inCount = case bsToAsts s of
                            A.Array a -> V.length a
                            _         -> error "Expected array"
                ceil    = ceiling :: Float -> Int

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
    name = "recurrentBucket";
    file = haskellVersion;
  };

  check = bucketCheck {
    inherit cmd;
    name = "recurrent";
    go   = "recurrentBucket";
  };
};

withDeps [ check ] cmd
