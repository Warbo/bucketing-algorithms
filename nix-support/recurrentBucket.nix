# Commands which split their input into various "buckets", e.g. based on
# clustering. We don't do any exploration or reduction, we just look at the
# resulting buckets.
{ bash, bc, bucketCheck, cluster, format, haskellPackages, jq, mkBin,
  runCommand, runWeka, stdenv, withDeps, wrap, writeScript }:

with rec {
  compile = { main, name }: runCommand "recurrent-bucket-${name}"
    {
      buildInputs = [
        (haskellPackages.ghcWithPackages (hs: [
        ]))
      ];
      main = writeScript "recurrent-bucket-${name}.hs" main;
    }
    ''
      cp "$main" Main.hs
      ghc --make -o Main Main.hs
      mv Main "$out"
    '';

  haskellPre = compile {
    name = "pre";
    main = ''
      module Main where
      import qualified Data.ByteString.Lazy.Char8 as LBS
      main = LBS.interact id
    '';
  };

  haskellPost = compile {
    name = "post";
    main = ''
      module Main where
      import qualified Data.ByteString.Lazy.Char8 as LBS
      main = LBS.interact id
    '';
  };

  cmd = mkBin {
    name   = "recurrentBucket";
    paths  = [ bash jq runWeka ];
    vars   = { inherit haskellPre haskellPost; SIMPLE = "1"; };
    script = ''
      #!/usr/bin/env bash
      set -e
      set -o pipefail

      # Allow empty input (as in ", not just "[]") for compatibility
      INPUT=$(cat)
      [[ -n "$INPUT" ]] || {
        echo "Empty input, nothing to cluster, short-circuiting" 1>&2
        echo '[]'
        exit 0
      }

      # Allow a single input (as in "{...}", not "[{...}]") for compatibility
      CHAR=$(echo "$INPUT" | head -n1 | cut -c 1)
      if [[ "x$CHAR" = "x{" ]]
      then
        echo "Input looks like a single object, wrapping into an array" 1>&2
        INPUT='['"$INPUT"']'
      fi

      CLUSTERED=$(echo "$INPUT" | "$haskellPre" | "${cluster}" | "$haskellPost")

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
