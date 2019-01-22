{ attrsToDirs', ghcWithML4HSFE, lib, nixpkgs1703, nixpkgs1803, runCommand,
  tebenchmark, wrap, writeScript }:

with builtins;
with lib;
with rec {
  bucketBoundBuilder = { cmds ? [], deps ? {}, main }:
    runCommand "bucket-bound-runner-${main}"
      (tebenchmark.cache // deps // {
        buildInputs = [
          (ghcWithML4HSFE { extraPkgs = [ "hashmap" "lens-aeson" ]; })
        ];
        src         = attrsToDirs' "bucket-bound-src" {
          "BucketBounds.hs"    = ../haskell-support/BucketBounds.hs;
          "BucketUtil.hs"      = ../haskell-support/BucketUtil.hs;
          "GetGroundTruths.hs" = ../haskell-support/GetGroundTruths.hs;
          "Helper.hs"          = ../haskell-support/GetGroundTruthsHelpers.hs;
          "Main.hs"            = writeScript "bb-main.hs" ''
            module Main where
            import qualified BucketBounds
            main = BucketBounds.${main}
          '';
        };
      })
      ''
        ${concatStringsSep "\n" cmds}
        cp -v "$src"/* ./
        ghc --make -O2 -o "$out" Main
      '';

  bucketBoundRunner = wrap {
    name  = "bucketBoundRunner";
    paths = [ nixpkgs1703.gecode nixpkgs1803.minizinc ];
    file  = bucketBoundBuilder {
      cmds = [ "$tests" ];
      deps = {
        tests = bucketBoundBuilder {
          main = "boundsTest";
        };
      };
      main = "boundsMain";
    };
  };
};
bucketBoundRunner
