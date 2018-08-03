# Commands useful for generating and bucketing test data. These have TEBenchmark
# baked in, so we only need to provide a sample of names and their corresponding
# ASTs will be looked up internally.
#
# We benchmark these in two ways:
#
#  - The impact of the bucketing algorithms is measured, on lots of test data
#  - The speed of the scripts is measured on small inputs, to aid us in
#    optimising their implementation (since the above can be very slow!)
{ bash, callPackage, hashBucket, jq, lib, recurrentBucket, wrap }:

with builtins;
rec {
  astsOf = callPackage ./astsOf.nix {};

  # Run the bucket script on each sample; we use a few bucket sizes, in
  # increments
  addHashBucketsCmd = wrap {
    name  = "hash";
    paths = [ hashBucket jq ];
    vars  = {
      inherit astsOf;
      sizes = concatStringsSep " " (map toString (lib.range 1 20));
    };
    script = ''
      #!/usr/bin/env bash
      set -e
      ASTS=$("$astsOf")

      for CLUSTER_SIZE in $sizes
      do
        export CLUSTER_SIZE
        echo "$ASTS" | hashBucket |
          jq '{(env["CLUSTER_SIZE"]) : map(map(.name))}'
      done | jq -s 'add'
    '';
  };

  addRecurrentBucketsCmd = wrap {
    name  = "recurrent";
    paths = [ recurrentBucket jq ];
    vars  = {
      inherit astsOf;
      sizes = concatStringsSep " " (map toString (lib.range 1 20));
    };
    script = ''
      #!/usr/bin/env bash
      set -e
      ASTS=$("$astsOf")

      for CLUSTER_SIZE in $sizes
      do
        export CLUSTER_SIZE
        echo "$ASTS" | recurrentBucket |
          jq '{(env["CLUSTER_SIZE"]) : map(map(.name))}'
      done | jq -s 'add'
    '';
  };

  makeDupeSamples = callPackage ./makeDupeSamples.nix {};

  dedupeSamples = callPackage ./dedupeSamples.nix { inherit makeDupeSamples; };

  getGroundTruths = callPackage ./getGroundTruths.nix {};
}
