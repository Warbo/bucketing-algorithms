allArgs:

# Provides an augmented package set to use instead of <nixpkgs>
with builtins;
with rec {
  # Default to a known, stable set of packages. Pass in 'false' to check against
  # the latest versions of everything.
  stable = allArgs.stable or true;

  # nix-config defines a bunch of stable package sets we can use
  inherit ((import <nixpkgs> { config = {}; }).callPackage ./nix-config.nix {
            inherit stable;
          })
          nix-config;

  pkgs = nix-config {
    args         = removeAttrs allArgs [ "stable" ];
    unstablePath = <nixpkgs>;
  };

  get = s: us: if stable then s else us;
};
assert pkgs ? unstable    || abort "No unstable nixpkgs found";
assert pkgs ? nixpkgs1603 || abort "No nixpkgs1603 found";
assert pkgs ? nixpkgs1609 || abort "No nixpkgs1609 found";
rec {
  inherit pkgs;
  inherit (pkgs) nixpkgs1709;
  nixpkgs-2016-03  = pkgs.nixpkgs1603;
  nixpkgs-2016-09  = pkgs.nixpkgs1609;
  nixpkgs          = get nixpkgs-2016-03             pkgs.unstable;
  nix-config       = get pkgs.customised.nixpkgs1603 pkgs.customised.unstable;
}
