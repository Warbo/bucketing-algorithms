# Commands useful for generating and bucketing test data. These have TEBenchmark
# baked in, so we only need to provide a sample of names and their corresponding
# ASTs will be looked up internally.
#
# We benchmark these in two ways:
#
#  - The impact of the bucketing algorithms is measured, on lots of test data
#  - The speed of the scripts is measured on small inputs, to aid us in
#    optimising their implementation (since the above can be very slow!)
{ bash, callPackage, hashBucket, jq, lib, parallel, recurrentBucket, wrap }:

with builtins;
rec {
  astsOf = callPackage ./astsOf.nix {};

  # Runs a bucketing script on samples from stdin. We use a few bucket sizes for
  # comparison. Since each bucket size is independent we use GNU Parallel to run
  # them concurrently; this ensures the outputs don't overlap, so 'jq' can
  # combine them correctly.
  skeleton = { cmd, dep, name }: wrap {
    inherit name;
    paths  = [ bash dep jq parallel ];
    vars   = {
      # Required for Perl
      LANGUAGE = "en_US.UTF-8";
      LC_ALL   = "en_US.UTF-8";
      LANG     = "en_US.UTF-8";
      LC_TYPE  = "en_US.UTF-8";

      # The string '{}' will be replaced by parallel with the size. The path to
      # the ASTs is dynamic, so we add it in between these two.
      pre  = ''export CLUSTER_SIZE={}; "${cmd}" < '';
      post = '' | jq '{"{}" : map(map(.name))}'
      '';
    };
    script = ''
      #!/usr/bin/env bash
      set -e

      # Store ASTs in /dev/shm so they're kept in memory
      FILENAME="/dev/shm/bucketing-$$-$RANDOM"
      function cleanUp {
        rm -f "$FILENAME"
      }
      trap cleanUp EXIT

      "${astsOf}" > "$FILENAME"

      parallel "$pre $FILENAME $post" ::: $(seq 1 20) | jq -s 'add'
    '';
  };

  addHashBucketsCmd = skeleton {
    name = "hash";
    cmd  = "hashBucket";
    dep  = hashBucket;
  };

  addRecurrentBucketsCmd = skeleton {
    name = "recurrent";
    cmd  = "recurrentBucket";
    dep  = recurrentBucket;
  };

  makeDupeSamples = callPackage ./makeDupeSamples.nix {};

  dedupeSamples = callPackage ./dedupeSamples.nix { inherit makeDupeSamples; };

  getGroundTruths = callPackage ./getGroundTruths.nix {};
}
