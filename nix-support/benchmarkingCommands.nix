# Commands useful for generating and bucketing test data. These have TEBenchmark
# baked in, so we only need to provide a sample of names and their corresponding
# ASTs will be looked up internally.
#
# We benchmark these in two ways:
#
#  - The impact of the bucketing algorithms is measured, on lots of test data
#  - The speed of the scripts is measured on small inputs, to aid us in
#    optimising their implementation (since the above can be very slow!)
{ bash, callPackage, fail, hashBucket, haskellPackages, jq, lib, makeSamples,
  python3, recurrentBucket, runCommand, tebenchmark, testData, withDeps, wrap,
  writeScript }:

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

  dedupeSamples = wrap {
    name = "dedupe.py";
    paths = [ python3 ];
    script = ''
      #!/usr/bin/env python3
      import json
      import sys

      data = json.loads(sys.stdin.read())
      for size in data:
        seen = []
        for iRep in sorted([int(rep) for rep in data[size]]):
          rep = str(iRep)
          sample = frozenset(data[size][rep])
          if sample in seen:
            data[size][rep] = None
          else:
            data[size][rep] = {'sample': data[size][rep]}
          seen += [sample]
      print(json.dumps(data))
    '';
  };

  getGroundTruths = callPackage ./getGroundTruths.nix {};
}
