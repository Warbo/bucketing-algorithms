{ bash, coreutils, jq, lib, nix, perl, procps, runCommand, utillinux }:
with builtins;
with rec {
  commonDeps = [ bash coreutils jq nix perl procps utillinux ];
};

# We must override buildInputs
env: env // { buildInputs = (env.buildInputs or []) ++ commonDeps; }
