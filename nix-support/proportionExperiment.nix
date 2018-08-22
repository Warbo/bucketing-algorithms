# Performs bucketing on a large set of samples, gets their ground truths,
# calculates the proportion remaining after bucketing and averages them.
{ attrsToDirs', bash, bucketProportions, composeBins, lzip, makeSamples, runOn,
  wrap }:

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

  go = { label ? "results", maxSize ? 100, reps ? 100 }: rec {
    steps = {
      "samples.json" = makeSamples { inherit maxSize reps; };

      "withBuckets.json.lz" =
        runOn "with-buckets"
              (composeBins "add-buckets-lzip" [ addBuckets lz ])
              steps."samples.json";

      "withBucketsGroundTruths.json.lz" =
        runOn "with-ground-truths"
              (composeBins "add-ground-truths-lzip" [ unlz getGroundTruths lz ])
              steps."withBuckets.json.lz";

      "withBucketsGroundTruthsProportions.json.lz" =
        runOn "with-proportions"
              (composeBins "proportions-lzip" [ unlz calculateProportions lz ])
              steps."withBucketsGroundTruths.json.lz";

      "averageBucketProportions.json.lz" =
        runOn "with-averages"
              (composeBins "average-lzip" [ unlz averageProportions lz ])
              steps."withBucketsGroundTruthsProportions.json.lz";
    };

    results = attrsToDirs' "proportion-experiment-${label}" steps;
  };


};
{
  results = go {};
  test    = go { label = "test"; maxSize = 2; reps = 30; };
}
