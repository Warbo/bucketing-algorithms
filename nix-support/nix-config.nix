{ defaultVersion, fetchFromGitHub }:

rec {
  nix-config     = import "${nix-config-src}" { inherit defaultVersion; };

  nix-config-src = fetchFromGitHub {
    owner  = "Warbo";
    repo   = "nix-config";
    rev    = "0133fb0";
    sha256 = "1c494608ldi285jv1axdkq8fy9kfzz0bh2w2kk1z65wdfnh1ywp6";
  };
}
