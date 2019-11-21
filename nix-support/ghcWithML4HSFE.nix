# ML4HSFE executable from its Haskell package, with its test script checked
{ fetchFromGitHub, fetchurl, haskell, jq, lib, nixpkgs1803, runCommand, unpack,
  withDeps }:

with builtins;
with lib;
with rec {
  getRepo = { name, owner ? "Warbo", patch ? (x: x), repo, rev, self, sha256 }:
    self.callPackage (nixpkgs1803.haskellPackages.haskellSrc2nix {
      inherit name;
      src = patch (fetchFromGitHub { inherit owner repo rev sha256; });
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
    nixpkgs1803.haskell.packages.ghc7103.override (old: {
      overrides = nixpkgs1803.lib.composeExtensions
        (old.overrides or (_: _: {}))
        (self: super:
          with {
            inherit (haskell.lib) doJailbreak dontCheck;

            # These nixpkgs definitions come from cabal2nix running with GHC 8,
            # where base contains Semigroup. Since we're using GHC 7, we need to
            # add a dependency on semigroups to some packages.
            forceSemigroups = p: haskell.lib.addBuildDepend p self.semigroups;
          };
          (if profile then profiler super else {}) // {
          # Tests can fail due to missing Arbitrary instances
          aeson      = dontCheck super.aeson;
          lens       = dontCheck super.lens;
          lens-aeson = dontCheck super.lens-aeson;

          # 1.4.8 on Hackage has old version bounds and is incompatible with
          # integer-gmp >= 1.0
          bitset = doJailbreak (getRepo {
            inherit self;
            name   = "bitset";
            owner  = "adamczykm";
            repo   = "bitset";
            rev    = "2d31c7f";
            sha256 = "09y4ffr4dbs5l9afsv515zx7w21ifabz8sfyz2qgbsf32vydxn91";
            patch  = raw: runCommand "bitset-patched"
              {
                inherit raw;
                buildInputs = [ self.hpack ];
              }
              ''
                cp -r "$raw" "$out"
                chmod +w -R "$out"
                cd "$out"
                hpack

                function go {
                  sed -i "src/Data/BitSet/Generic.hs" -e "s/$1/$2/g"
                }
                go ".*Semigroup.*" ""
                go ".*(<>).*"      ""
                go ".*mempty.*"    ""
                go "instance Bits c => Monoid (BitSet c a) where" \
                   "${concatStrings [
                      "instance Bits c => Monoid (BitSet c a) where { "
                      "mempty  = empty; "
                      "mappend = union; "
                      "}"
                    ]}"
              '';
          });

          # Newer versions depend on QuickCheck >= 2.10
          cassava = self.callHackage "cassava" "0.4.5.1" {};

          # Dependency of bitset
          conduit = forceSemigroups super.conduit;

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
          # error. Version 2.9 hits problems elsewhere, with semigroups,
          # instances and quickcheck-io packages.
          QuickCheck = self.callHackage "QuickCheck" "2.8.2" {};

          # Versions above this need QuickCHeck > 2.8
          quickcheck-instances =
            self.callHackage "quickcheck-instances" "0.3.13" {};

          # We use QuickSpec version 1 (0.9.6) since version 2+ is very different
          quickspec = self.callHackage "quickspec" "0.9.6" {};

          system-filepath = forceSemigroups super.system-filepath;

          # "isAscii" tests fail for empty strings; seems harmless
          text-short = forceSemigroups (dontCheck super.text-short);
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
