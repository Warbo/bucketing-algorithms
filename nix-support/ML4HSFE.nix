# ML4HSFE executable from its Haskell package, with its test script checked
{ fetchFromGitHub, haskell, jq, nixpkgs1803, runCabal2nix, runCommand,
  withDeps }:

with rec {
  # Override haskellPackages entries to ensure compatible dependencies are used
  # for ML4HSFE and the other libraries we want to use with it. We do this here
  # rather than in haskellPackages, since there's no point trying to coordinate
  # all dependencies of all our Haskell packages to be mutually compatible with
  # each other. If these packages' dependencies break other stuff, we don't need
  # to care since we're not using these for anything else.
  hsPkgs = haskell.packages.ghc7103.override (old: {
    overrides = nixpkgs1803.lib.composeExtensions
      (old.overrides or (_: _: {}))
      (self: super: {
        ML4HSFE = runCabal2nix {
          name = "ML4HSFE";
          url  = fetchFromGitHub {
            owner  = "Warbo";
            repo   = "ml4hsfe";
            rev    = "e4e4cea";
            sha256 = "1kcnhbkgfp0akp0g0jxh11f1zn96jybgl7rniwabhxpr9hszj3kn";
          };
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
  # TODO: Rename to ghcWithML4HSFE
  def   = env;
  tests = {
    inherit env;

    # ML4HSFE also provides its own test script, outside of its Cabal test suite
    testSuite = runCommand "ML4HSFE-test-suite"
      {
        ml4hsfe     = def.src;
        buildInputs = [
          (haskellPackages.ghcWithPackages (h: [
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
