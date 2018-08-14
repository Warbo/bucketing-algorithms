# Scripts for bucketing samples in a variety of ways and measure the
# proportion of ground truth theorems which apply to the resulting buckets.
#
# Write output to JSON for archiving.
{ attrsToDirs', averageProportions, benchmarkingCommands, calculateProportions,
  callPackage, ghcWithML4HSFE, runCommand, wrap, writeScript }:

with { inherit (builtins) concatStringsSep map; };
with callPackage ./astsOf.nix {};
with rec {
  # Runs each sample through all bucketers, adding the result to the samples
  # JSON
  addBuckets = wrap {
    name  = "process-samples";
    vars  = { LANG = "en_US.UTF-8"; };
    file  = runCommand "process-samples-script"
      {
        buildInputs = [ ghcWithML4HSFE ];
        mods        = attrsToDirs' "bucket-proportions-mods" (astsOfModules // {
          "BucketUtil.hs"      = ../haskell-support/BucketUtil.hs;
          "HashBucket.hs"      = ../haskell-support/HashBucket.hs;
          "RecurrentBucket.hs" = ../haskell-support/RecurrentBucket.hs;
          "Main.hs"            = writeScript "process-samples-main.hs" ''
            module Main where

            import qualified Data.Aeson as A
            import qualified BucketUtil
            import qualified HashBucket
            import qualified RecurrentBucket

            bucketers = [HashBucket.bucketer, RecurrentBucket.bucketer]

            go = BucketUtil.bucketAll bucketers
                                      (BucketUtil.astsOf AstsOf.astsOf')

            main = do
              i <- LBS.getContents
              case A.eitherDecode i of
                Left err -> error err
                Right ss -> LBS.putStr (A.encode (go ss))
          '';
        });
      }
      ''
        cp -rv "$mods" mods
        chmod +w -R mods
        cd mods
        ghc --make -o "$out" Main.hs
      '';
  };
};

rec {
  inherit addBuckets averageProportions calculateProportions;
  inherit (benchmarkingCommands) getGroundTruths;
}
