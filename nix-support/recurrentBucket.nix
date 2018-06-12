# Commands which split their input into various "buckets", e.g. based on
# clustering. We don't do any exploration or reduction, we just look at the
# resulting buckets.
{ bash, bc, bucketCheck, cluster, format, jq, mkBin, ghc, runCommand, runWeka,
  stdenv, withDeps, wrap, writeScript }:

with rec {
  cmd = mkBin {
    name   = "recurrentBucket";
    paths  = [ bash jq runWeka ];
    vars   = { SIMPLE = "1"; };
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

      CLUSTERED=$(echo "$INPUT" | ${cluster})

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
