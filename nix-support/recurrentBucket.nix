# Commands which split their input into various "buckets", e.g. based on
# clustering. We don't do any exploration or reduction, we just look at the
# resulting buckets.
{ bash, bc, callPackage, cluster, format, jq, mkBin, ghc, runCommand, runWeka,
  stdenv, withDeps, wrap, writeScript }:

with rec {
  go = mkBin {
    name   = "recurrentBucket";
    paths  = [ bash jq runWeka ];
    vars   = { SIMPLE = "1"; };
    script = ''
      #!/usr/bin/env bash
      set -e
      set -o pipefail

      # Perform clustering
      CLUSTERED=$(${cluster})

      clCount=$(echo "$CLUSTERED" | jq 'map(.cluster) | max')
      export clCount

      echo "$CLUSTERED" | "${format.fromStdin}"
    '';
  };
};

withDeps [] go
