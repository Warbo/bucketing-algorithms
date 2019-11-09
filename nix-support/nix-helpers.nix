{ fetchFromGitHub, lib, path }:

with {
  defs = import ./addOverlayToNixpkgs.nix {
    inherit lib path;
    src = fetchFromGitHub {
      owner  = "Warbo";
      repo   = "nix-helpers";
      rev    = "7c3a983";
      sha256 = "1hhwhwm5nf0d3r4x9rd9hkm5xhmfk43gcwzc4caiglc02m4snhvg";
    };
  };
};
defs.nix-helpers
