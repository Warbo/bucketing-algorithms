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
with rec {
  # Imports a file and calls it as a function with args from 'self' (allowing
  # both 'self' and 'super' to be given as args if desired). The result is
  # overridable by default; to avoid this we can return a set with a 'def'
  # attribute; this also allows 'tests' to be returned alongside.
  call = f: self.newScope { inherit self super; }
                          (./nix-support + "/${f}.nix")
                          {};

  appendDef = n: old:
    with rec {
      wrap   = x: { "${n}" = x; };
      result = call n;
      def    = wrap (result.def or result);
      test   = if result ? def && result ? tests then wrap result.tests else {};
      tests  = {
        bucketing-algorithms-tests = old.bucketing-algorithms-tests // test;
      };
    };
    old // def // tests // {
      bucketing-algorithms = old.bucketing-algorithms // def // tests;
    };
};
fold appendDef { bucketing-algorithms = {}; bucketing-algorithms-tests = {}; } [
  "averageProportions"
  "benchmark"
  "benchmarkingCommands"
  "bucketCheck"
  "bucketProportions"
  "calculateProportions"
  "callHackage"
  "hashBucket"
  "haskellPackages"
  "haskellPkgToAsts"
  "haskellPkgToRawAsts"
  "haskellSources"
  "haskellTE"
  "hsOverride"
  "makeSamples"
  "ML4HSFE"
  "nix-helpers"
  "package"
  "performance"
  "recurrentBucket"
  "tebenchmark"
] // {
  # The main nixpkgs repo, augmented with nix-config, depending on un/stable
  inherit (import ./nixpkgs.nix {})
    nix-config nix-config-src nixpkgs;

  inherit (self.nix-helpers)
    # Pristine releases of nixpkgs. Useful for avoiding known incompatibilities.
    nixpkgs1603 nixpkgs1609 nixpkgs1709 nixpkgs1803

    # Helper functions, etc.
    allDrvsIn asv attrsToDirs backtrace composeWithArgs die fail inNixedDir
    latestGit mkBin nixListToBashArray nothing pipeToNix repo reverse
    runCabal2nix sanitiseName stableHackageDb stripOverrides timeout tryElse
    unlines unpack withDeps wrap;

  # Fixed versions to avoid known breakages

  inherit (self.nixpkgs1603)
    # Args differ in new versions, which breaks ./nix-support/haskellPackages.nix scripts
    cabal2nix;

  inherit (self.nixpkgs1609)
    # The quoting is different in other versions, which breaks e.g. wrap
    makeWrapper

    # Old versions don't have the needed contracts, new ones don't build on i686
    racket;

  # Cases where we want both the attribute set and its attributes available

  inherit (self.haskellTE)
    testData;
}
