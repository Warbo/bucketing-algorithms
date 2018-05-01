# Builds the environment in which to run a benchmark
args:

with builtins;
with import ../nix-support {};
with lib;
with callPackage ./bucketProportions.nix {
  maxSize = 20;
  reps    = 10;
};

mkBin {
  name   = "python";
  paths  = [ (nixpkgs1609.python3.withPackages (p: [])) ];
  vars   = { inherit result; };
  script = ''
    #!/usr/bin/env bash
    exec python3 "$@"
  '';
}
