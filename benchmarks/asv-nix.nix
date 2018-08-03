{ asv, python3Packages, warbo-packages }:

warbo-packages.asv-nix.override {
  asv            = asv.override { pythonPackages = python3Packages; };
  pythonPackages = python3Packages;
}
