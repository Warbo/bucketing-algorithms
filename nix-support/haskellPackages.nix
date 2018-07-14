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
  value = hsPkgs.override { overrides = hsOverride (_: _: {}); };
  removeOverrides = true;  # Otherwise they'd mess up the Haskell overrides
}
