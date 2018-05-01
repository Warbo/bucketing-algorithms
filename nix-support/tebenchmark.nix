{ latestGit, haskellPackages, nix-config-src, repo }:

import (latestGit {
  url    = http://chriswarbo.net/git/theory-exploration-benchmarks.git;
  stable = {
    rev    = "eb9f5b9";
    sha256 = "1jfqjc6s8lgp6ndqrhv4abzcsda2wrf2rwwj740y1lgam609wwzy";
  };
}) {
  inherit haskellPackages nix-config-src;
  pkgsPath = repo;
  pkgsArgs = { config = import "${nix-config-src}/custom.nix"; };
}
