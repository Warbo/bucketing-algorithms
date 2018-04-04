{ fetchgit, haskellPackages, nix-config-src, repo }:

import (fetchgit {
  url    = http://chriswarbo.net/git/theory-exploration-benchmarks.git;
  rev    = "eb9f5b9";
  sha256 = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855";
}) {
  inherit haskellPackages nix-config-src;
  pkgsPath = repo;
  pkgsArgs = { config = import "${nix-config-src}/custom.nix"; };
}
