# Useful for ASTs, etc.
{ latestGit, nix-helpers }:

latestGit {
  url    = "https://github.com/Warbo/haskell-te.git";
  stable = {
    rev        = "24475a2";
    sha256     = "0w1mfvqfkh0hvzbcks4pnj56n9w6xvifydhqvqfb894mj2cajyaf";
    unsafeSkip = false;
  };
}
