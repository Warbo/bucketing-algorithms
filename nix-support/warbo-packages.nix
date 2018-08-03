{ fetchFromGitHub, lib, path }:

with {
  defs = import ./addOverlayToNixpkgs.nix {
    inherit lib path;
    src = fetchFromGitHub {
      owner  = "Warbo";
      repo   = "warbo-packages";
      rev    = "fadf087";
      sha256 = "0z4jk3wk9lhlq3njr22wsr9plf5fw7mmpbky8l8ppn0gp698vq63";
    };
  };
};
defs.warbo-packages
