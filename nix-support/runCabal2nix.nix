# Generates derivations which run cabal2nix, which we can import to get a usable
# Haskell package function. We take this from nix-helpers, but override the
# cabal2nix version to that of nixpkgs1803 since that accepts a --compiler arg.
{ nix-helpers, nixpkgs1803 }:

nix-helpers.runCabal2nix.override {
  inherit (nixpkgs1803) cabal2nix;
}
