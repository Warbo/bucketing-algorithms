{ stable ? true }:

# Provides an augmented package set to use instead of <nixpkgs>
with builtins;
with rec {
  # The release of nixpkgs we should use by default
  defaultVersion = "nixpkgs1603";

  # nix-config defines a bunch of package sets we can use
  configured = (import <nixpkgs> { config = {}; }).callPackage ./nix-config.nix
                 { inherit defaultVersion stable; };
};

configured // {
  # As well as the overridden packages, we also provide a pristine version of
  # our default nixpkgs set
  nixpkgs = getAttr (if stable then defaultVersion else "unstable")
                    configured.nix-config;
}
