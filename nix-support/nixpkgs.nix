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
          nix-config nix-config-src;

  get = s: us: if stable then s else us;
};
assert nix-config ? unstable    || abort "No unstable nixpkgs found";
assert nix-config ? nixpkgs1603 || abort "No nixpkgs1603 found";
assert nix-config ? nixpkgs1609 || abort "No nixpkgs1609 found";
rec {
  inherit nix-config nix-config-src;
  inherit (pkgs) nixpkgs1709;
  nixpkgs-2016-03  = pkgs.nixpkgs1603;
  nixpkgs-2016-09  = pkgs.nixpkgs1609;
  nixpkgs          = pkgs;
  pkgs             = nix-config.customised.nixpkgs1603;
}
