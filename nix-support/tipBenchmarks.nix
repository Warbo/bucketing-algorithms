{ bash, callPackage, defaultClusters, fetchgit, fetchurl,
  haskellPackages, nixFromCabal, processPackage, python, racket, runScript,
  stdenv, storeResult, writeScript }:

with builtins;
let path = if any (x: x.prefix == "te-benchmarks") nixPath
              then <te-benchmarks>
              else fetchgit {
                     url    = "https://github.com/Warbo/theory-exploration-benchmarks.git";
                     rev    = "40bccd9";
                     sha256 = "188q7hqa83kan0ycndmzz8clswjync9404049scjsxd2vgg9yw23";
                     fetchSubmodules = true;
                   };
 in rec {
  inherit (callPackage path {})
    tip-benchmarks tools tip-benchmark-smtlib;

  # Uses tip-benchmark-smtlib to produce a Haskell package
  tip-benchmarks-haskell = stdenv.mkDerivation {
    name         = "tip-benchmarks-haskell";
    buildInputs  = [ tools ];
                    SMT_FILE     = tip-benchmark-smtlib;
    buildCommand = ''
      source $stdenv/setup
      set -e

      export OUT_DIR="$out"
      mkdir -p "$OUT_DIR"

      # Create Haskell package
      full_haskell_package.rkt < "${tip-benchmark-smtlib}"
    '';
  };

  pkgDef = nixFromCabal (toString tip-benchmarks-haskell) null;

  pkg = haskellPackages.callPackage pkgDef {};

  process = { clusters ? defaultClusters, sampleSize ? null }:
              processPackage { inherit clusters sampleSize; }
                             pkg.name pkg;
}
