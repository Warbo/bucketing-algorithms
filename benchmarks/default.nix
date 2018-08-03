# Builds the environment in which to run a benchmark. This will be called from
# asv, passing in dependencies as arguments.
{
  dir  ? ./.., # Path to the revision containing the benchmarks
  root ? ./.., # Path to the revision being benchmarked
  ...
}:

with builtins;
with trace (toJSON { inherit dir root; }) {
  fixed    = import "${dir }";
  measured = import "${root}";
};
with fixed.nixpkgs1609.lib;

fixed.mkBin {
  name  = "python";
  paths = with fixed.nixpkgs1609; [ (python3.withPackages (p: [])) ];
  vars  = {
    inherit root;
    LANG = "en_US.UTF-8";

    # All of the scripts to benchmark should be in here, taken from measured
    commands = toJSON {
      inherit (measured.benchmarkingCommands)
        addHashBucketsCmd addRecurrentBucketsCmd astsOf dedupeSamples
        getGroundTruths;

      sample = with fixed; wrap {
        name   = "sample";
        vars   = {
          sizes = toJSON (range 1 100);
          reps  = "100";
        };
        script = ''
          #!/usr/bin/env bash

          function stripExpected {
            # This removes the stderr messages we expect from the sampler, since
            # there will be a lot of them and ASV will dump them all out.
            grep -v '^ *Sampling [0-9]* names from a total of [0-9]*$' |
            grep -v '^ *Converted all theorem dependencies into constraints$'
            grep -v '^ *Calculated frequency for each constraint$'
            grep -v '^ *Shuffling names$'
            grep -v '^ *Obtained sample$'
            grep -v '^ *Size [0-9]* rep [0-9]*$'
          }

          "${measured.benchmarkingCommands.makeDupeSamples}" \
            2> >(stripExpected 1>&2)
        '';
      };
    };

    # A fixed set of samples, for scripts which need them as input
    samples = fixed.makeSamples {
      sizes = [ 1 5 10 15 20 ];
      reps  = 5;
    };
  };
  script = ''
    #!/usr/bin/env bash
    exec python3 "$@"
  '';
}
