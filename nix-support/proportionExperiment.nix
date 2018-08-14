# Performs bucketing on a large set of samples, gets their ground truths,
# calculates the proportion remaining after bucketing and averages them.
{ attrsToDirs', bucketProportions, makeSamples, runOn }:

with bucketProportions;
with rec {
  results = {
    "samples.json" = makeSamples {
      maxSize = 100;
      reps    = 100;
    };

    "withBuckets.json" =
      runOn "with-buckets"
            addBuckets
            results."samples.json";

    "withBucketsGroundTruths.json" =
      runOn "with-ground-truths"
            getGroundTruths
            results."withBuckets.json";

    "withBucketsGroundTruthsProportions.json" =
      runOn "with-proportions"
            calculateProportions
            results."withBucketsGroundTruths.json";

    "averageBucketProportions.json" =
      runOn "with-averages"
            averageProportions
            results."withBucketsGroundTruthsProportions.json";
  };
};
{
  steps   = results;
  results = attrsToDirs' "proportion-experiment-results" results;
}
