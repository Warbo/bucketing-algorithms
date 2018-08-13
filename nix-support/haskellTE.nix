# Useful for ASTs, etc.
{ latestGit, nix-helpers }:

with {
  src = latestGit {
    url    = "https://github.com/Warbo/haskell-te.git";
    stable = {
      rev        = "5e98338";
      sha256     = "1wlm17kkhblmq7d9yzfmqs4n9qilw8bxmyznv4lbqh2rl3311l5w";
      unsafeSkip = false;
    };
  };
};
import nix-helpers.repo1803 { overlays = [ (import "${src}/overlay.nix") ]; }
