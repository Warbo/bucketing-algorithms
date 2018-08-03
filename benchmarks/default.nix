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
        name = "sample";
        script = ''
          #!/usr/bin/env bash
          export sizes="[$1]"
          export  reps="$2"
          exec "${measured.benchmarkingCommands.makeDupeSamples}"
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
