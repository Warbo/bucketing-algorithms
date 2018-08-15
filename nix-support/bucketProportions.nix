# Scripts for bucketing samples in a variety of ways and measure the
# proportion of ground truth theorems which apply to the resulting buckets.
#
# Write output to JSON for archiving.
{ attrsToDirs', averageProportions, benchmarkingCommands, calculateProportions,
  callPackage, ghcWithML4HSFE, makeSamples, runCommand, wrap, writeScript }:

with { inherit (builtins) concatStringsSep map; };
with callPackage ./astsOf.nix {};
with rec {
  # Runs each sample through all bucketers, adding the result to the samples
  # JSON
  addBuckets = { profile ? false }: wrap {
    name  = "process-samples${if profile then "prof" else ""}";
    vars  = { LANG = "en_US.UTF-8"; };
    file  = runCommand "process-samples-script${if profile then "prof" else ""}"
      {
        buildInputs = [ (ghcWithML4HSFE { inherit profile; }) ];
        mods        = attrsToDirs' "bucket-proportions-mods" (astsOfModules // {
          "BucketUtil.hs"      = ../haskell-support/BucketUtil.hs;
          "HashBucket.hs"      = ../haskell-support/HashBucket.hs;
          "RecurrentBucket.hs" = ../haskell-support/RecurrentBucket.hs;
          "Main.hs"            = writeScript "process-samples-main.hs" ''
            module Main where

            import qualified AstsOf
            import qualified BucketUtil
            import qualified Data.Aeson                 as A
            import qualified Data.ByteString.Lazy.Char8 as LBS
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
        ghc --make -o "$out" ${if profile
                                  then "-prof -prof-all"
                                  else ""} Main.hs
      '';
  };
};

{
  inherit averageProportions calculateProportions;
  inherit (benchmarkingCommands) getGroundTruths;
  addBuckets = addBuckets {};

  # Useful for tracking down misbehaving bucketers, etc.
  memoryProfile = runCommand "bucketing-memory-profile"
    {
      prog = addBuckets { profile = true; };
    }
    ''
      mkdir "$out"
      cd "$out"
      "$prog" -h < "${makeSamples { maxSize = 10; reps = 10; }}" > /dev/null
    '';
}
