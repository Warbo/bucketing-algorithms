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
        import           System.Exit
        import           System.IO
        import qualified System.Process             as P
        import qualified System.Process.ByteString  as PB

        cluster s = do
            (c, o, e) <- PB.readCreateProcessWithExitCode cmd s
            BS.hPutStr stderr e
            case c of
                 ExitSuccess   -> LBS.putStr (processAsts o)
                 ExitFailure n -> error ("Non-zero exit code " ++ show n)
          where cmd = P.proc "${cluster}" []

        bsToAsts :: BS.ByteString -> A.Value
        bsToAsts s = case A.eitherDecode (LBS.fromStrict s) of
                       Left  e -> error ("Failed to read ASTs " ++ e)
                       Right x -> x

        processAsts = A.encode . H.elems . splitUp . bsToAsts
          where splitUp v = case v of
                              A.Array a -> V.foldl' acc H.empty a
                              _         -> error ("Expected arr got " ++ show v)

                acc h v = case v of
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
                     else cluster i
      '';
    }
    ''
      cp "$main" Main.hs
      ghc --make -o Main Main.hs
      mv Main "$out"
    '';

  cmd = mkBin {
    name   = "recurrentBucket";
    paths  = [ bash jq runWeka ];
    vars   = { inherit haskellVersion; SIMPLE = "1"; };
    script = ''
      #!/usr/bin/env bash
      set -e
      set -o pipefail

      # Allow empty input (as in ", not just "[]") for compatibility
      INPUT=$(cat)

      # Allow a single input (as in "{...}", not "[{...}]") for compatibility
      CHAR=$(echo "$INPUT" | head -n1 | cut -c 1)
      if [[ "x$CHAR" = "x{" ]]
      then
        echo "Input looks like a single object, wrapping into an array" 1>&2
        INPUT='['"$INPUT"']'
      fi

      echo "$INPUT" | "$haskellVersion"
    '';
  };

  check = bucketCheck {
    inherit cmd;
    name = "recurrent";
    go   = "recurrentBucket";
  };
};

withDeps [ check ] cmd
