{ bash, coreutils, jq, nix, perl, procps, utillinux }:
with builtins;
with rec {
  commonDeps = [ bash coreutils jq nix perl procps utillinux ];
};

# We must override buildInputs
env: env // { buildInputs = (env.buildInputs or []) ++ commonDeps; }
