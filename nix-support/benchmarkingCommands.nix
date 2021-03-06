# Commands useful for generating and bucketing test data. These have TEBenchmark
# baked in, so we only need to provide a sample of names and their corresponding
# ASTs will be looked up internally.
#
# We benchmark these in two ways:
#
#  - The impact of the bucketing algorithms is measured, on lots of test data
#  - The speed of the scripts is measured on small inputs, to aid us in
#    optimising their implementation (since the above can be very slow!)
{ attrsToDirs', bash, callPackage, ghcWithML4HSFE, jq, lib, runCommand, wrap,
  writeScript }:

with builtins;
with callPackage ./astsOf.nix {};
rec {
  astsOf = astsOfScript;

  cmdSkeleton = { buildInputs, mod, name }: wrap {
    inherit name;
    file = runCommand "${name}-compiled"
      {
        inherit buildInputs;
        __noChroot = true;
        main       = writeScript "${name}-main.hs" ''
          module Main where

          import qualified AstsOf
          import qualified BucketUtil
          import qualified Data.Aeson                 as A
          import qualified Data.ByteString.Lazy.Char8 as LBS
          import qualified Data.Text.Lazy             as T
          import qualified ${mod}

          main = do
            i <- LBS.getContents
            let names = case A.eitherDecode i of
                  Left err -> error err
                  Right ns -> ns
                asts     = BucketUtil.astsOf AstsOf.astsOf' names
                bucketed = BucketUtil.bucketCounts [1..20] asts ${mod}.bucketer
                entries  = BucketUtil.entries bucketed
            LBS.putStr (A.encode (BucketUtil.toJSON' (head entries)))
        '';
      }
      ''
        cp -v "${attrsToDirs' "asts-of-mods" astsOfModules}"/* ./
        cp -v "${attrsToDirs' "bucketing-mods" {
          "BucketUtil.hs" = ../haskell-support/BucketUtil.hs;
          "HashBucket.hs" = ../haskell-support/HashBucket.hs;
          "RecurrentBucket.hs" = ../haskell-support/RecurrentBucket.hs;
        }}"/* ./
        cp -v "$main" Main.hs
        ghc --make -O2 -o "$out" Main.hs
      '';
  };

  addHashBucketsCmd = cmdSkeleton {
    buildInputs = [ (ghcWithML4HSFE {}) ];
    mod         = "HashBucket";
    name        = "hash";
  };

  addRecurrentBucketsCmd = cmdSkeleton {
    buildInputs = [ (ghcWithML4HSFE {}) ];
    mod         = "RecurrentBucket";
    name        = "recurrent";
  };

  makeDupeSamples = callPackage ./makeDupeSamples.nix {};

  dedupeSamples = callPackage ./dedupeSamples.nix { inherit makeDupeSamples; };

  getGroundTruths = callPackage ./getGroundTruths.nix {};
}
