# Builds the environment in which to run a benchmark. This will be called from
# asv, passing in dependencies as arguments.
{
  dir  ? ./.., # Path to the revision containing the benchmarks
  root ? ./.., # Path to the revision being benchmarked
  ...
}:

with builtins;
with trace (toJSON { inherit dir root; }) {
  fixed    = import "${dir }/nix-support" {};
  measured = import "${root}/nix-support" {};
};
with fixed.lib;

fixed.mkBin {
  name  = "python";
  paths = with fixed.nixpkgs1609; [ (python3.withPackages (p: [])) ];
  vars  = {
    inherit root;

    # All of the scripts to benchmark should be in here, taken from measured
    commands = toJSON {
      inherit (measured.benchmarkingCommands)
        addHashBucketsCmd addRecurrentBucketsCmd astsOf dedupeSamples
        getGroundTruths;
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
