# Builds the environment in which to run a benchmark. This will be called from
# asv, passing in dependencies as arguments.
{
  dir,  # Path to the revision being benchmarked
  ...
}:

with builtins;
with {
  fixed    = import ../nix-support {};
  measured = import "${dir}/nix-support" {};
};
with lib;

mkBin {
  name  = "python3";
  paths = [ (fixed.nixpkgs1609.python3.withPackages (p: [])) ];
  vars  = {
    # All of the scripts to benchmark should be in here, taken from measured
    commands = toJSON {
      inherit (measured.benchmarkingCommands) addHashBucketsCmd;
    };

    # A fixed set of samples, for scripts which need them as input
    samples = fixed.makeSamples {
      maxSize = 20;
      reps    = 10;
    };
  };
  script = ''
    #!/usr/bin/env bash
    exec python3 "$@"
  '';
}
