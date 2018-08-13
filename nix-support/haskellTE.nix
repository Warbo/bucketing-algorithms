# Useful for ASTs, etc.
{ latestGit, nix, nix-helpers }:

with {
  src = latestGit {
    url    = "https://github.com/Warbo/haskell-te.git";
    stable = {
      rev        = "c68e6fd";
      sha256     = "14ji8kzk6z477kq8vf633iqm57v1z9byz630mmq51ppikvgjmisi";
      unsafeSkip = false;
    };
  };
};
import nix-helpers.repo1803 {
  overlays = [
    (import "${src}/overlay.nix")
    (self: super: { inherit nix; })
  ];
}
