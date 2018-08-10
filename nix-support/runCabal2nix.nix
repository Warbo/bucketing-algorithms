# Generates derivations which run cabal2nix, which we can import to get a usable
# Haskell package function. We take this from nix-helpers, but override the
# cabal2nix version to that of nixpkgs1803 since that accepts a --compiler arg.
{ cabal2nix, nix-helpers }:

nix-helpers.runCabal2nix.override { inherit cabal2nix; }
