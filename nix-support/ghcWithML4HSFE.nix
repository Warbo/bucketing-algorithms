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

  getUrl = { name, self, sha256, url }: run {
    inherit name self;
    url = unpack (fetchurl { inherit url sha256; });
  };

  run = { name, self, url }:
    self.callPackage (runCabal2nix { inherit name url; }) {};

  # Override haskellPackages entries to ensure compatible dependencies are used
  # for ML4HSFE and the other libraries we want to use with it. We do this here
  # rather than in haskellPackages, since there's no point trying to coordinate
  # all dependencies of all our Haskell packages to be mutually compatible with
  # each other. If these packages' dependencies break other stuff, we don't need
  # to care since we're not using these for anything else.
  hsPkgs = nixpkgs1803.haskell.packages.ghc802.override (old: {
    overrides = nixpkgs1803.lib.composeExtensions
      (old.overrides or (_: _: {}))
      (self: super: {
        HS2AST = getRepo {
          inherit self;
          name   = "HS2AST";
          repo   = "hs2ast";
          rev    = "469d999";
          sha256 = "1x2f12s6caj0gaymaw62bmm62ydim78wm2pn18j18fa2l3p7vqyi";
        };

        ML4HSFE = getRepo {
          inherit self;
          name   = "ML4HSFE";
          repo   = "ml4hsfe";
          rev    = "e4e4cea";
          sha256 = "1kcnhbkgfp0akp0g0jxh11f1zn96jybgl7rniwabhxpr9hszj3kn";
        };

        # Needed by twee-lib
        /*primitive = getUrl {
          inherit self;
          name   = "primitive";
          url    = "https://hackage.haskell.org/package/primitive-0.6.2.0/primitive-0.6.2.0.tar.gz";
          sha256 = "1q9a537av81c0lvcdzc8i5hqjx3209f5448d1smkyaz22c1dgs5q";
        };*/

        # quickspec 0.9.6 encounters ambiguous 'total' with 2.10+, whilst 2.9
        # needs semigroups
        QuickCheck = getUrl rec {
          inherit self;
          name   = "QuickCheck";
          url    = "https://hackage.haskell.org/package/QuickCheck-2.9.2/QuickCheck-2.9.2.tar.gz";
          sha256 = "119np67qvx8hyp9vkg4gr2wv3lj3j6ay2vl4hxspkg43ymb1cp0m";
        };

        # 2.1.5 fails by assuming semigroup pkg
        /*twee-lib = getUrl {
          inherit self;
          name   = "twee-lib";
          url    = "https://hackage.haskell.org/package/twee-lib-2.1.4/twee-lib-2.1.4.tar.gz";
          sha256 = "0fapi6g8f6fp90hvff1g606wzi7jaacf1mnq1h801yzzi4sj3yms";
        };*/
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
