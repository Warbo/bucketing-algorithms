{ callPackage, die, latestGit, lib }:

with builtins;
with rec {
  runCabal2nix = callPackage ./runCabal2nix.nix {};

  get = name:
    {
      spec   ? null,
      url    ? null,
      owner  ? "Warbo",
      repo   ? null,
      rev    ? null,
      sha256 ? null
    }:
      assert spec != null || url != null || repo != null || die {
        inherit name owner repo rev sha256 spec url;
        error = "Need URL or repo or spec";
      };
      assert !(elem null [ url repo ]) -> !(elem null [ rev sha256 ]) || die {
        inherit name owner repo rev sha256 spec url;
        error = "Using (GitHub) repo or (Git) url requires rev and sha256";
      };
      with rec {
        stable  = {
          inherit rev sha256;
          unsafeSkip = false;
        };
        fullUrl = if url == null
                     then "https://github.com/${owner}/${repo}.git"
                     else url;
        src     = latestGit { inherit stable; url = fullUrl; };
      };
      runCabal2nix {
        inherit name;
        url = if spec == null then src else spec;
      };
};
lib.mapAttrs get {
  # Only needed for dependency solving

  #semigroups = { spec = "cabal://semigroups-0.11"; };
  tasty      = { spec = "cabal://tasty-0.11.2.1";  };

  # Specific versions of other people's packages, e.g. with patches we need

  ifcxt = {
    owner  = "mikeizbicki";
    repo   = "ifcxt";
    rev    = "7f9f876";
    sha256 = "0mzd5h45rkvj81pdi60p68r0j3lc4h9m4z3b4v8m6xacp9sxiic1";
  };

  spoon = {
    repo   = "spoon";
    rev    = "4424f9a";
    sha256 = "14mn6ygr0wqy4css8wrbxd6b4qvp951xgc206x79fjfva3q6n12g";
  };

  # Our packages

  HS2AST = {
    repo   = "hs2ast";
    rev    = "469d999";
    sha256 = "1x2f12s6caj0gaymaw62bmm62ydim78wm2pn18j18fa2l3p7vqyi";
  };

  ML4HSFE = {
    repo   = "ml4hsfe";
    rev    = "e4e4cea";
    sha256 = "1kcnhbkgfp0akp0g0jxh11f1zn96jybgl7rniwabhxpr9hszj3kn";
  };

  runtime-arbitrary = {
    repo   = "runtime-arbitrary";
    rev    = "5b7ff2f";
    sha256 = "11gnfmz48vxvf42xs9255r51vbv1sjghvzi60gcrpx3jk38d2gyb";
  };
}
