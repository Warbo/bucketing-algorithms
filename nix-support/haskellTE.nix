# Useful for ASTs, etc.
{ haskellTESrc, nix-helpers }:

import nix-helpers.repo1803 {
  overlays = [ (import "${haskellTESrc}/overlay.nix") ];
}
