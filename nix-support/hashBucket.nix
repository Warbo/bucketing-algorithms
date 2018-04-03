{ nixedHsPkg, nixpkgs1709 }:

nixpkgs1709.haskellPackages.callPackage (nixedHsPkg ../hash-bucket) {}
