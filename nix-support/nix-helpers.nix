{ fetchFromGitHub, lib, path }:

with builtins;
with rec {
  src = fetchFromGitHub {
    owner  = "Warbo";
    repo   = "nix-helpers";
    rev    = "197a47f";
    sha256 = "1xy271k4zbb5gpkfdql2yz9ffzcd6l0bqsx08f0p2jaz8zkqm05g";
  };

  oldImport = lib.fix (self: import path {
    config = { packageOverrides = import "${src}/overlay.nix" self; };
  });

  newImport = import path { overlays = [ (import "${src}/overlay.nix") ]; };

  version = (import path {}).lib.nixpkgsVersion;

  defs = if builtins.compareVersions version "17.03" == -1
            then oldImport
            else newImport;
};

defs.nix-helpers
