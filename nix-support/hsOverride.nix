{ callHackage, haskellSources, lib }:

with builtins;
with lib;

with rec {
  hsPkgs = { hackagePkg, self }: {
    # Only needed for dependency solving

    semigroups = hackagePkg "semigroups" "0.11"     {};
    tasty      = hackagePkg "tasty"      "0.11.2.1" {};
  } // mapAttrs (_: p: self.callPackage p {}) haskellSources;
};

extra: self: super: hsPkgs {
  inherit self;
  hackagePkg = n: v: self.callPackage (callHackage n v);
} // extra self super
