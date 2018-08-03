{ fetchFromGitHub, lib, path }:

with {
  defs = import ./addOverlayToNixpkgs.nix {
    inherit lib path;
    src = fetchFromGitHub {
      owner  = "Warbo";
      repo   = "nix-helpers";
      rev    = "72d9d88";
      sha256 = "1kggqr07dz2widv895wp8g1x314lqg19p67nzr3b97pg97amhjsi";
    };
  };
};
defs.nix-helpers
