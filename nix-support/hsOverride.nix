{ callHackage, latestGit, lib, nixedHsPkg, stable }:

with builtins;
with lib;

with rec {
  hsPkgs = { get, hackagePkg }: {

    # Only needed for dependency solving

    semigroups = hackagePkg "semigroups" "0.11"     {};
    tasty      = hackagePkg "tasty"      "0.11.2.1" {};

    # Specific versions of other people's packages, e.g. with patches we need

    ifcxt = get {
      path   = <ifcxt>;
      owner  = "mikeizbicki";
      repo   = "ifcxt";
      rev    = "7f9f876";
      sha256 = "0mzd5h45rkvj81pdi60p68r0j3lc4h9m4z3b4v8m6xacp9sxiic1";
    } {};

    spoon = get {
      repo   = "spoon";
      rev    = "4424f9a";
      sha256 = "14mn6ygr0wqy4css8wrbxd6b4qvp951xgc206x79fjfva3q6n12g";
    } {};

    # Our packages

    HS2AST = get {
      path   = <hs2ast>;
      repo   = "hs2ast";
      rev    = "469d999";
      sha256 = "1x2f12s6caj0gaymaw62bmm62ydim78wm2pn18j18fa2l3p7vqyi";
    } {};

    ML4HSFE = get {
      path   = <ml4hsfe>;
      repo   = "ml4hsfe";
      rev    = "e4e4cea";
      sha256 = "1kcnhbkgfp0akp0g0jxh11f1zn96jybgl7rniwabhxpr9hszj3kn";
    } {};

    runtime-arbitrary = get {
      path   = <runtime-arbitrary>;
      repo   = "runtime-arbitrary";
      rev    = "5b7ff2f";
      sha256 = "11gnfmz48vxvf42xs9255r51vbv1sjghvzi60gcrpx3jk38d2gyb";
    } {};
  };
};

extra: self: super: hsPkgs {
  hackagePkg = n: v: self.callPackage (callHackage n v);

  get = { path ? null, url ? null, owner ? "Warbo", repo ? null, rev, sha256}:
    assert url == null -> repo != null || abort "Need URL or repo (${sha256})";
    with rec {
      stable  = { inherit rev sha256; };
      fullUrl = if url == null
                   then "https://github.com/${owner}/${repo}.git"
                   else url;
      git     = latestGit { inherit stable; url = fullUrl; };
      src     = with tryEval path;
                if success && value != null then value else git;
    };
    self.callPackage (nixedHsPkg (toString src));
} // extra self super
