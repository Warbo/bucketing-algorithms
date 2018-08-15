# Command to read annotated ASTs from stdin, and write them to stdout grouped
# into "buckets". Chooses what to put in each bucket arbitrarily by using a
# hash of the names.
{ bucketCheck, bucketRunner, fail, ghcWithML4HSFE, jq, mkBin, runCommand,
  withDeps, writeScript }:
with rec {
  hashBucket = bucketRunner {
    buildInputs = [ (ghcWithML4HSFE {}) ];
    files = {
      "BucketUtil.hs" = ../haskell-support/BucketUtil.hs;
      "HashBucket.hs" = ../haskell-support/HashBucket.hs;
    };
    mod  = "HashBucket";
    name = "hashBucket";
  };

  cmd = mkBin {
    name = "hashBucket";
    file = hashBucket;
  };

  hashCheck = bucketCheck {
    inherit cmd;
    name = "hash";
    go   = "hashBucket";
  };
};
withDeps [ hashCheck ] cmd
