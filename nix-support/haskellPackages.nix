{ hsOverride, nixpkgs1803, stable }:

with builtins;
with rec {
  hsPkgs     = nixpkgs1803.haskell.packages.ghc7103;
  ghcVersion = hsPkgs.ghc.version;
  reqVersion = "7.10.3";
};

assert stable -> ghcVersion == reqVersion ||
       abort "Stable build requires GHC ${reqVersion}, using GHC ${ghcVersion}";

{
  value = hsPkgs.override { overrides = hsOverride (_: _: {}); };
  removeOverrides = true;  # Otherwise they'd mess up the Haskell overrides
}
