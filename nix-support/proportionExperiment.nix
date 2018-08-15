# Performs bucketing on a large set of samples, gets their ground truths,
# calculates the proportion remaining after bucketing and averages them.
{ attrsToDirs', bucketProportions, makeSamples, runOn }:

with bucketProportions;
with rec {
  go = { label ? "results", maxSize ? 100, reps ? 100 }: rec {
    steps = {
      "samples.json" = makeSamples { inherit maxSize reps; };

      "withBuckets.json" =
        runOn "with-buckets"
              addBuckets
              steps."samples.json";

      "withBucketsGroundTruths.json" =
        runOn "with-ground-truths"
              getGroundTruths
              steps."withBuckets.json";

      "withBucketsGroundTruthsProportions.json" =
        runOn "with-proportions"
              calculateProportions
              steps."withBucketsGroundTruths.json";

      "averageBucketProportions.json" =
        runOn "with-averages"
              averageProportions
              steps."withBucketsGroundTruthsProportions.json";
    };

    results = attrsToDirs' "proportion-experiment-${label}" steps;
  };


};
{
  results = go {};
  test    = go { label = "test"; maxSize = 2; reps = 30; };
}
