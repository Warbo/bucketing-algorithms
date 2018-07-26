# ML4HSFE executable from its Haskell package, with its test script checked
{ fetchFromGitHub,fetchurl,  haskell, jq, nixpkgs1803, runCabal2nix, runCommand,
  unpack, withDeps }:

with builtins;
with rec {
  getRepo = { name, repo, rev, self, sha256 }: run {
    inherit name self;
    url = fetchFromGitHub {
      inherit repo rev sha256;
      owner = "Warbo";
    };
  };

  getHackage = { name, self, sha256, version }:
    with { nv = "${name}-${version}"; };
    run {
      inherit name self;
      url = unpack (fetchurl {
        inherit sha256;
        url = "https://hackage.haskell.org/package/${nv}/${nv}.tar.gz";
      });
    };

  run = { name, self, url }:
    self.callPackage (runCabal2nix { inherit name url; }) {};

  # Override haskellPackages entries to ensure compatible dependencies are used
  # for ML4HSFE and the other libraries we want to use with it. We do this here
  # rather than in haskellPackages, since there's no point trying to coordinate
  # all dependencies of all our Haskell packages to be mutually compatible with
  # each other. If these packages' dependencies break other stuff, we don't need
  # to care since we're not using these for anything else.
  hsPkgs = nixpkgs1803.haskell.packages.ghc7103.override (old: {
    overrides = nixpkgs1803.lib.composeExtensions
      (old.overrides or (_: _: {}))
      (self: super: {
        # Tests can fail due to missing Arbitrary instances
        aeson = haskell.lib.dontCheck super.aeson;

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
          rev    = "e4e4cea";
          sha256 = "1kcnhbkgfp0akp0g0jxh11f1zn96jybgl7rniwabhxpr9hszj3kn";
        };

        # If this is 2.10+ then quickspec 0.9.6 hits an "ambiguous 'total'"
        # error. Version 2.9 hits problems elsewhere, with semigroups, instances
        # and quickcheck-io packages.
        QuickCheck = getHackage {
          inherit self;
          name    = "QuickCheck";
          version = "2.8.2";
          sha256  = "1ai6k5v0bibaxq8xffcblc6rwmmk6gf8vjyd9p2h3y6vwbhlvilq";
        };

        # We use QuickSpec version 1 (0.9.6) since version 2+ is very different
        quickspec = getHackage {
          inherit self;
          name    = "quickspec";
          version = "0.9.6";
          sha256  = "0prwzxsrvfqryl75rmma229d4y7ra61vc3d72kyqi4l44ga2ay21";
        };

        # This package's dependencies differ depending on whether we're using
        # GHC 7 (semigroups dependency is added) or GHC 8 (no semigroups
        # dependency). The nixpkgs package is the same across all of its Haskell
        # package sets; in particular, no semigroups dependency is passed in,
        # even if we're using GHC 7 that needs it.
        system-filepath = getHackage {
          inherit self;
          name    = "system-filepath";
          version = "0.4.13.4";
          sha256  = "1yy5zsmmimhg6iaw9fmpwrxvxrgi5s6bfyqfihdsnx4bjvn7sp9l";
        };
      });
  });

  # These are the packages we want to ensure are working
  env = hsPkgs.ghcWithPackages (hs: [
    hs.aeson
    hs.ML4HSFE
    hs.process
    hs.process-extras
    hs.unordered-containers
    hs.vector
  ]);
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
