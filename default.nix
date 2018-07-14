# Applies our overlay to a known-good version of nixpkgs

# We use the system's <nixpkgs> to bootstrap, so we can access nix-helpers
with import <nixpkgs> { overlays = [ (import ./overlay.nix) ]; };

# We expose a version pinned to a nixpkgs version provided by nix-helpers. Note
# that since this version predates nixpkgs1703, it doesn't provide the overlays
# API, so we emulate it using the old packageOverrides API. If this version gets
# bumped in the future, it can be simplified to just use the overlay directly.
import nix-helpers.repo1603 {
  config = {
    packageOverrides = super:
      super.lib.fix (self: import ./overlay.nix self super);
  };
}
