# Builds the environment in which to run a benchmark
with builtins;
with import ../nix-support {};
with lib;

mkBin {
  name  = "python3";
  paths = [ (nixpkgs1609.python3.withPackages (p: [])) ];
  vars  = {
    # All of the scripts to benchmark should go in here
    commands = toJSON { inherit (benchmarkingCommands) addHashBucketsCmd; };

    # A fixed set of samples, for scripts which need them as input
    samples = makeSamples {
      maxSize = 20;
      reps    = 10;
    };
  };
  script = ''
    #!/usr/bin/env bash
    exec python3 "$@"
  '';
}
