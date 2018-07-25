{ fetchFromGitHub, lib, path }:

with {
  defs = import ./addOverlayToNixpkgs.nix {
    inherit lib path;
    src = fetchFromGitHub {
      owner  = "Warbo";
      repo   = "nix-helpers";
      rev    = "785b2be";
      sha256 = "0yybw870dqzkh7qq91winlsncy2j9zq34bc2zl04727rqy4ph8n5";
    };
  };
};
defs.nix-helpers
