{ stable ? true }:

# Provides an augmented package set to use instead of <nixpkgs>
with builtins;
with rec {
  # The release of nixpkgs we should use by default
  defaultVersion = if stable then "nixpkgs1603" else "unstable";

  # nix-config defines a bunch of package sets we can use
  configured = (import <nixpkgs> { config = {}; }).callPackage ./nix-config.nix
                 { inherit defaultVersion stable; };
};

configured // {
  # As well as the overridden packages, we also provide a pristine version of
  # our default nixpkgs set
  nixpkgs = getAttr defaultVersion configured.nix-config;
}
