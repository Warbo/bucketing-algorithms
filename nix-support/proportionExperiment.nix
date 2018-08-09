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

    "withRecurrent.json" =
      runOn "with-recurrent-buckets"
            addRecurrent
            results."samples.json";

    "withRecurrentHashed.json" =
      runOn "with-hash-buckets"
            addHashed
            results."withRecurrent.json";

    "withRecurrentHashedGroundTruths.json" =
      runOn "with-ground-truths"
            getGroundTruths
            results."withRecurrentHashed.json";

    "withRecurrentHashedGroundTruthsProportions.json" =
      runOn "with-proportions"
            calculateProportions
            results."withRecurrentHashedGroundTruths.json";

    "averageRecurrentHashed.json" =
      runOn "with-averages"
            averageProportions
            results."withRecurrentHashedGroundTruthsProportions.json";
  };
};
{
  steps   = results;
  results = attrsToDirs' "proportion-experiment-results" results;
}
