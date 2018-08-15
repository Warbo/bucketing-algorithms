# Commands which split their input into various "buckets", e.g. based on
# clustering. We don't do any exploration or reduction, we just look at the
# resulting buckets.
{ bash, bucketCheck, bucketRunner, ghcWithML4HSFE, mkBin, runCommand, withDeps,
  wrap, writeScript }:

with rec {
  haskellVersion = bucketRunner {
    buildInputs = [ (ghcWithML4HSFE {}) ];
    files       = {
      "BucketUtil.hs"      = ../haskell-support/BucketUtil.hs;
      "RecurrentBucket.hs" = ../haskell-support/RecurrentBucket.hs;
    };
    mod  = "RecurrentBucket";
    name = "recurrent-bucket";
  };

  cmd = mkBin {
    name = "recurrentBucket";
    file = haskellVersion;
  };

  check = bucketCheck {
    inherit cmd;
    name = "recurrent";
    go   = "recurrentBucket";
  };
};

withDeps [ check ] cmd
