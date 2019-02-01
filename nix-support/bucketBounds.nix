{ attrsToDirs', extractSampleJSON, ghcWithML4HSFE, jq, lib, nixpkgs1703,
  nixpkgs1803, runCommand, tebenchmark, wrap, writeScript }:

with builtins;
with lib;
with rec {
  bucketBoundBuilder = { cmds ? [], deps ? {}, main }:
    runCommand "bucket-bound-runner-${main}"
      (tebenchmark.cache // deps // {
        buildInputs = [
          (ghcWithML4HSFE { extraPkgs = [ "hashmap" "lens-aeson" ]; })
        ];
        src         = attrsToDirs' "bucket-bound-src" {
          "BucketBounds.hs"    = ../haskell-support/BucketBounds.hs;
          "BucketUtil.hs"      = ../haskell-support/BucketUtil.hs;
          "GetGroundTruths.hs" = ../haskell-support/GetGroundTruths.hs;
          "Helper.hs"          = ../haskell-support/GetGroundTruthsHelpers.hs;
          "Main.hs"            = writeScript "bb-main.hs" ''
            module Main where
            import qualified BucketBounds
            main = BucketBounds.${main}
          '';
        };
      })
      ''
        ${concatStringsSep "\n" cmds}
        cp -v "$src"/* ./
        ghc --make -O2 -o "$out" Main
      '';

  bucketBoundRunner = wrap {
    name  = "bucketBoundRunner";
    paths = [ nixpkgs1703.gecode nixpkgs1803.minizinc ];
    file  = bucketBoundBuilder {
      cmds = [ "$tests" ];
      deps = {
        tests = bucketBoundBuilder {
          main = "boundsTest";
        };
      };
      main = "boundsMain";
    };
  };

  data = runCommand "bucketBounds"
    {
      inherit bucketBoundRunner;
      buildInputs = [ jq ];
      extracted   = extractSampleJSON;
    }
    ''
      # Start off with an empty object (no sizes)
      echo '{}' > result.json

      export SAMPLE_JSON="$PWD/input.json"
      for SIZE in $(seq 1 10)
      do
        echo "Extracting samples of size $SIZE" 1>&2
        jq --arg size "$SIZE" '{($size) : .[$size]}' < "$extracted" \
                                                     > "$SAMPLE_JSON"

        echo "Processing size $SIZE" 1>&2
        date 1>&2
        "$bucketBoundRunner" > output.json
        date 1>&2

        # Merge this size into the result
        jq --argfile result result.json '$result * .' < output.json > temp.json
        mv temp.json result.json
      done

      jq 'map_values(map_values(.sample))' < result.json > "$out"
    '';
};
data
