# Defines a nixpkgs overlay: this is intended to be used like:
#
#   import <nixpkgs> { overlays = [ (import ./overlay.nix) ]; }
#
# Note that you might prefer ./default.nix or ./release.nix to do this for you.
#
# An overlay defines a bunch of attributes which get added to those of nixpkgs.
# Its arguments are 'super' (the original nixpkgs set) and 'self' (the nixpkgs
# set augmented with these new definitions). Note that 'self' requires laziness
# to avoid an infinite loop! In general, our definitions should take their
# dependencies from 'self', since this will propagate overrides, etc. The two
# main exceptions, used to avoid infinite loops, are:
#
#  - If we're overriding something, e.g. replacing 'foo' with 'foo.override...',
#    but keeping the same name (e.g. 'foo'), we should take the original from
#    'super', i.e. 'super.foo.override...'. This is because 'self.foo' refers to
#    our override, which is not the thing we want to override!
#  - The names defined by our overlay cannot depend on 'self', since this would
#    introduce a circular dependency. For example, if we did:
#      self: super: self.lib.mapAttrs myFunc myAttrs
#    This is an infinite loop, since the result of this call might include a
#    'lib' attribute, in which case that's the 'lib.mapAttrs' we'd be calling.
#    Note that this is not just Nix being naive: we might think "I know how
#    'mapAttrs' (or whatever) works, and it won't produce a 'lib' override, but
#    Nix isn't able to spot that". In fact, we *don't* know how 'mapAttrs' works
#    since this is a constraint not a definition. Valid solutions include e.g.
#      with rec { mapAttrs = _: _: { lib = { inherit mapAttrs; }; }; }; mapAttrs
#    This ignores its input and overrides 'lib', yet it fits the circular logic
#    above! This is why we must break such loops by using 'super' instead.
#
# Note that referring to 'self' in our *values* is fine, as long as we avoid
# self-reference (pun intended), since values cannot affect their names, so they
# cannot introduce ambiguity or "global circularity" (self-reference can cause
# "local circularity", but that only breaks that definition and its dependents).
self: super:

# Our policy is to define things in separate files in nix-support/, taking
# their arguments from 'self // { inherit super; }'. We only include "global"
# definitions, i.e. things we want to expose to users and things which are
# useful across multiple definitions. It's fine for a nix-support/ file to only
# be imported by those things which use it, as this keeps scopes narrow.

with builtins;
with super.lib;

{
  # The main nixpkgs repo, augmented with nix-config, depending on un/stable
  inherit (import ./nixpkgs.nix {})
    nix-config nix-config-src nixpkgs;

  inherit (nix-config)
    # Pristine releases of nixpkgs. Useful for avoiding known incompatibilities.
    nixpkgs1603 nixpkgs1609 nixpkgs1709 nixpkgs1803

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

  # Imports a file and calls the function it contains, automatically looking up
  # argument values from the 'self' attrset.
  callPackage = nixpkgs.callPackage ./callPackage.nix { inherit self; };

  averageProportions    = callPackage ./averageProportions.nix    {};
  benchmarkingCommands  = callPackage ./benchmarkingCommands.nix  {};
  bucketCheck           = callPackage ./bucketCheck.nix           {};
  bucketProportions     = callPackage ./bucketProportions.nix     {};
  calculateProportions  = callPackage ./calculateProportions.nix  {};
  callHackage           = callPackage ./callHackage.nix           {};
  hashBucket            = callPackage ./hashBucket.nix            {};
  haskellPackages       = callPackage ./haskellPackages.nix       {};
  haskellPkgToAsts      = callPackage ./haskellPkgToAsts.nix      {};
  haskellPkgToRawAsts   = callPackage ./haskellPkgToRawAsts.nix   {};
  haskellSources        = callPackage ./haskellSources.nix        {};
  haskellTE             = callPackage ./haskellTE.nix             {};
  hsOverride            = callPackage ./hsOverride.nix            {};
  makeSamples           = callPackage ./makeSamples.nix           {};
  ML4HSFE               = callPackage ./ML4HSFE.nix               {};
  nixedHsPkg            = callPackage ./nixedHsPkg.nix            {};
  package               = callPackage ./package.nix               {};
  recurrentBucket       = callPackage ./recurrentBucket.nix       {};
  tebenchmark           = callPackage ./tebenchmark.nix           {};
})
