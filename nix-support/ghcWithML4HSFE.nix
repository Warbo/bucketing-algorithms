# ML4HSFE executable from its Haskell package, with its test script checked
{ fetchFromGitHub, fetchurl, haskell, jq, nixpkgs1803, runCommand, unpack,
  withDeps }:

with builtins;
with rec {
  getRepo = { name, repo, rev, self, sha256 }:
    self.callPackage (nixpkgs1803.haskellPackages.haskellSrc2nix {
      inherit name;
      src = fetchFromGitHub {
        inherit repo rev sha256;
        owner = "Warbo";
      };
    }) {};

  # If added to Haskell overrides, will force all packages to be profiled
  profiler = super: {
    mkDerivation = args: super.mkDerivation (args // {
      enableLibraryProfiling = true;
    });
  };

  # Override haskellPackages entries to ensure compatible dependencies are used
  # for ML4HSFE and the other libraries we want to use with it. We do this here
  # rather than in haskellPackages, since there's no point trying to coordinate
  # all dependencies of all our Haskell packages to be mutually compatible with
  # each other. If these packages' dependencies break other stuff, we don't need
  # to care since we're not using these for anything else.
  hsPkgs = { profile ? false }:
    with { inherit (nixpkgs1803.lib) composeExtensions; };
    nixpkgs1803.haskell.packages.ghc7103.override (old: {
      overrides = composeExtensions
        (old.overrides or (_: _: {}))
        (self: super: (if profile then profiler super else {}) // {
          # Tests can fail due to missing Arbitrary instances
          aeson      = haskell.lib.dontCheck super.aeson;
          lens       = haskell.lib.dontCheck super.lens;
          lens-aeson = haskell.lib.dontCheck super.lens-aeson;

          # Dependency of ML4HSFE. Note that this needs GHC 7.10, due to changes
          # in GHC's package DB implementation.
          HS2AST = getRepo {
            inherit self;
            name   = "HS2AST";
            repo   = "hs2ast";
            rev    = "469d999";
            sha256 = "1x2f12s6caj0gaymaw62bmm62ydim78wm2pn18j18fa2l3p7vqyi";
          };

          # This is the package we want, all of the rest are dependencies.
          ML4HSFE = getRepo {
            inherit self;
            name   = "ML4HSFE";
            repo   = "ml4hsfe";
            rev    = "a6635a3";
            sha256 = "1gf9rpzwdn8pwicf581w2kx9cwzr8aqancajnilc638zy3787z02";
          };

          # If this is 2.10+ then quickspec 0.9.6 hits an "ambiguous 'total'"
          # error. Version 2.9 hits problems elsewhere, with semigroups, instances
          # and quickcheck-io packages.
          QuickCheck = self.callHackage "QuickCheck" "2.8.2" {};

          # We use QuickSpec version 1 (0.9.6) since version 2+ is very different
          quickspec = self.callHackage "quickspec" "0.9.6" {};

          # This package was converted to Nix in the context of GHC 8, but it
          # depends on semigroups when using GHC 7.
          system-filepath = haskell.lib.addBuildDepend super.system-filepath
                                                       self.semigroups;
        });
    });

  # These are the packages we want to ensure are working
  env = { extraPkgs ? [], profile ? false }:
    (hsPkgs { inherit profile; }).ghcWithPackages
      (hs: map (n: getAttr n hs) (extraPkgs ++ [
        "aeson"
        "bytestring"
        "containers"
        "cryptonite"
        "intern"
        "memory"
        "ML4HSFE"
        "process"
        "process-extras"
        "QuickCheck"
        "tasty"
        "tasty-quickcheck"
        "text"
        "th-lift-instances"
        "unordered-containers"
        "vector"
      ]));
};
rec {
  def   = env;
  tests = {
    inherit env;

    # ML4HSFE also provides its own test script, outside of its Cabal test suite
    testSuite = runCommand "ML4HSFE-test-suite"
      {
        ml4hsfe     = def.src;
        buildInputs = [
          (hsPkgs.ghcWithPackages (h: [
            h.cabal-install h.ghc h.kmeans h.ML4HSFE h.quickspec h.tasty
            h.tasty-quickcheck
        ]))
        jq
      ];
      }
    ''
      export HOME="$PWD"
      cp -r "$ml4hsfe" ./ml4hsfe
      chmod +w -R ./ml4hsfe
      cd ./ml4hsfe
      bash ./test.sh
      mkdir "$out"
    '';
  };
}
