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
          hs.process
          hs.process-extras
        ]))
      ];
      main = writeScript "recurrent-bucket-main.hs" ''
        {-# LANGUAGE OverloadedStrings #-}
        module Main where
        import qualified Data.ByteString.Char8      as BS
        import qualified Data.Char                  as C
        import           System.Exit
        import           System.IO
        import qualified System.Process             as P
        import qualified System.Process.ByteString  as PB

        cluster s = do
            (c, o, e) <- PB.readCreateProcessWithExitCode cmd s
            BS.hPutStr stderr e
            case c of
                 ExitSuccess   -> BS.putStr o
                 ExitFailure n -> error ("Non-zero exit code " ++ show n)
          where cmd = P.proc "${cluster}" []

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

      CLUSTERED=$(echo "$INPUT" | "$haskellVersion")

      clCount=$(echo "$CLUSTERED" | jq 'map(.cluster) | max')
      export clCount

      echo "$CLUSTERED" | jq 'map(del(.tocluster))' |
                          "${format.fromStdin}"
    '';
  };

  check = bucketCheck {
    inherit cmd;
    name = "recurrent";
    go   = "recurrentBucket";
  };
};

withDeps [ check ] cmd
