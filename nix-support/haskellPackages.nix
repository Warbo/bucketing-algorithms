{ die, hsOverride, nixpkgs1803 }:

with builtins;
with rec {
  hsPkgs   = nixpkgs1803.haskell.packages.ghc7103;
  given    = hsPkgs.ghc.version;
  required = "7.10.3";
};

assert given == required || die {
  inherit given required;
  error = "Wrong GHC version for bucketing-algorithms";
};
{
  def = hsPkgs.override (old: {
    overrides = nixpkgs1803.lib.composeExtensions
                  (old.overrides or (_: _: {}))
                  hsOverride;
  });
}
