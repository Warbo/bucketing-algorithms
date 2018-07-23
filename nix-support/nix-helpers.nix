{ fetchFromGitHub, lib, path }:

with builtins;
with rec {
  src = fetchFromGitHub {
    owner  = "Warbo";
    repo   = "nix-helpers";
    rev    = "96a2fa3";
    sha256 = "0j5xxgjbyjsj9ayj3q7b95s7gzmmahwlj27nvbmdjyrxk3dn7gxz";
  };

  oldImport = lib.fix (self: import path {
    config = { packageOverrides = import "${src}/overlay.nix" self; };
  });

  newImport = import path { overlays = [ (import "${src}/overlay.nix") ]; };

  defs = if builtins.compareVersions lib.nixpkgsVersion "17.03" == -1
            then oldImport
            else newImport;
};

defs.nix-helpers
