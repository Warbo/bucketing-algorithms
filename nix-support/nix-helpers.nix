{ fetchFromGitHub, lib, path }:

with {
  defs = import ./addOverlayToNixpkgs.nix {
    inherit lib path;
    src = fetchFromGitHub {
      owner  = "Warbo";
      repo   = "nix-helpers";
      rev    = "197a47f";
      sha256 = "1xy271k4zbb5gpkfdql2yz9ffzcd6l0bqsx08f0p2jaz8zkqm05g";
    };
  };
};
defs.nix-helpers
