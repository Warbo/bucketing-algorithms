# All of our "global" definitions live here (i.e. everything that's used in more
# than one place). Note that care should be taken to avoid infinite loops, since
# 'callPackage' gets arguments from 'self', which is the set we're defining!
{
  lib    ? (import <nixpkgs> { config = {}; }).lib,
  stable ? true,
  ...
}@args:

with builtins;
with lib;

fix (self: rec {
  # Whether to use the latest packages or known-good versions
  inherit stable;

  # The main nixpkgs repo, augmented with nix-config, depending on un/stable
  inherit (import ./nixpkgs.nix { inherit stable; })
    nix-config nix-config-src nixpkgs;

  inherit (nixpkgs)
    # Regular dependencies, used as-is
    bash buildEnv cabal-install glibcLocales jq lib runCommand stdenv utillinux
    writeScript;

  inherit (nix-config)
    # Pristine releases of nixpkgs. Useful for avoiding known incompatibilities.
    nixpkgs1603 nixpkgs1609 nixpkgs1709

    # Helper functions, etc.
    allDrvsIn asv attrsToDirs backtrace composeWithArgs fail inNixedDir
    latestGit mkBin nixListToBashArray nothing pipeToNix repo reverse
    sanitiseName stableHackageDb stripOverrides timeout tryElse unlines unpack
    withDeps wrap;

  # Fixed versions to avoid known breakages

  inherit (nixpkgs1603)
    # Args differ in new versions, which breaks ./haskellPackages.nix scripts
    cabal2nix;

  inherit (nixpkgs1609)
    # The quoting is different in other versions, which breaks e.g. wrap
    makeWrapper

    # Old versions don't have the needed contracts, new ones don't build on i686
    racket;

  # Cases where we want both the attribute set and its attributes available

  inherit (haskellTE)
    testData;

  inherit (runTypesScriptData)
    runTypesScript;

  # Imports a file and calls the function it contains, automatically looking up
  # argument values from the 'self' attrset.
  callPackage = nixpkgs.callPackage ./callPackage.nix { inherit self; };

  benchmarkingCommands  = callPackage ./benchmarkingCommands.nix  {};
  bucketCheck           = callPackage ./bucketCheck.nix {};
  bucketProportions     = callPackage ./bucketProportions.nix     {};
  callHackage           = callPackage ./callHackage.nix           {};
  checkHsEnv            = callPackage ./checkHsEnv.nix            {};
  checkStderr           = callPackage ./checkStderr.nix           {};
  extraHaskellPackages  = callPackage ./extraHaskellPackages.nix  {};
  filterToSampled       = callPackage ./filterToSampled.nix       {};
  getDepsScript         = callPackage ./getDepsScript.nix         {};
  hashBucket            = callPackage ./hashBucket.nix            {};
  haskellPackages       = callPackage ./haskellPackages.nix       {};
  haskellPkgNameVersion = callPackage ./haskellPkgNameVersion.nix {};
  haskellPkgToAsts      = callPackage ./haskellPkgToAsts.nix      {};
  haskellPkgToRawAsts   = callPackage ./haskellPkgToRawAsts.nix   {};
  haskellTE             = callPackage ./haskellTE.nix             {};
  haveVar               = callPackage ./haveVar.nix               {};
  hsNameVersion         = callPackage ./hsNameVersion.nix         {};
  hsOverride            = callPackage ./hsOverride.nix            {};
  makeHaskellPkgNixable = callPackage ./makeHaskellPkgNixable.nix {};
  makeSamples           = callPackage ./makeSamples.nix           {};
  ML4HSFE               = callPackage ./ML4HSFE.nix               {};
  nixedHsPkg            = callPackage ./nixedHsPkg.nix            {};
  package               = callPackage ./package.nix               {};
  pkgName               = callPackage ./pkgName.nix               {};
  recurrentBucket       = callPackage ./recurrentBucket.nix       {};
  runWeka               = callPackage ./runWeka.nix               {};
  tebenchmark           = callPackage ./tebenchmark.nix           {};
  tryTip                = callPackage ./tryTip.nix                {};
  withNix               = callPackage ./withNix.nix               {};
})
