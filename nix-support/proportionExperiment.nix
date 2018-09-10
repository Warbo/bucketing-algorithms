# Performs bucketing on a large set of samples, gets their ground truths,
# calculates the proportion remaining after bucketing and averages them.
{ attrsToDirs', bash, bucketProportions, callPackage, composeBins, coreutils,
  lzip, makeSamples, msgpack-tools, nixpkgs1803, pv, runCommand, runOn,
  withDeps, wrap }:

with bucketProportions;
with rec {
  lz = "${lzip}/bin/lzip";

  unlz = wrap {
    name   = "unlzip";
    paths  = [ bash lzip ];
    script = ''
      #!/usr/bin/env bash
      exec lzip -d "$@"
    '';
  };

  go = { label ? "results", maxSize ? 100, reps ? 100, deps ? [] }: rec {
    steps = {
      "samples.json" = withDeps deps (makeSamples { inherit maxSize reps; });

      "withBuckets.json.lz" =
        runOn "with-buckets"
              (composeBins "add-buckets-lzip" [ addBuckets lz ])
              steps."samples.json";

      "withBucketsGroundTruths.json.lz" =
        runOn "with-ground-truths"
              (composeBins "add-ground-truths-lzip" [ unlz getGroundTruths lz ])
              steps."withBuckets.json.lz";

      "withBucketsGroundTruths.msgpack.lz" = runCommand
        "withBucketsGroundTruths.msgpack.lz"
        {
          buildInputs = [
            msgpack-tools
            coreutils
            lzip
            pv  # Give progress bars
          ];
          f = steps."withBucketsGroundTruths.json.lz";
        }
        ''
          echo "Extracting '$f' temporarily" 1>&2
          pv -f -p -e "$f" | lzip -d > extracted.json

          echo "Converting JSON to MsgPack" 1>&2
          S=$(stat --printf="%s" "$f")
          json2msgpack extracted.json | lzip | pv -f -p -e -s "$S" > "$out"
        '';

      "withBucketsGroundTruthsProportions.msgpack.lz" =
        runOn "with-proportions"
              (composeBins "proportions-lzip" [ unlz calculateProportions lz ])
              steps."withBucketsGroundTruths.msgpack.lz";

      "averageBucketProportions.msgpack.lz" =
        runOn "with-averages"
              (composeBins "average-lzip" [ unlz averageProportions lz ])
              steps."withBucketsGroundTruthsProportions.msgpack.lz";
    };

    results = attrsToDirs' "proportion-experiment-${label}" steps;
  };

  test = go { label = "test"; maxSize = 2; reps = 30; };
};
{
  inherit test;
  results = go { deps = [ test.results ]; };
}
