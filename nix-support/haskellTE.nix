# Useful for ASTs, etc.
{ latestGit, stable }:

with {
  src = latestGit {
    url    = "https://github.com/Warbo/haskell-te.git";
    stable = {
      rev    = "8904b29";
      sha256 = "059h0jh01ay1kw2l0wh2i3sqni8bc4k3ankrh7cf0v624knbqb09";
    };
  };
};

import "${src}/nix-support" { inherit stable; }