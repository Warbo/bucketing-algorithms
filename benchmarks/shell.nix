with import ../nix-support {};
stdenv.mkDerivation (nix-config.withNix {
  name         = "haskell-te-benchmark-env";
  src          = ./shell.nix;
  unpackPhase  = "true";
  buildInputs  = [ (callPackage ./asv-nix.nix {}) nixpkgs1609.git ];
  installPhase = ''
    set -e
    echo 'WARNING: Building the "benchmarkEnv" derivation. This is not meant
    to be a "real" package: it only provides a build environment, for use with
    nix-shell'
    echo "This is not a 'package', it's meant to be used via nix-shell" > "$out"
  '';
  shellHook = ''
    echo "Entered benchmarking shell: use 'asv' command to run benchmarks." 1>&2
  '';
})
