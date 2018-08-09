# Commands which split their input into various "buckets", e.g. based on
# clustering. We don't do any exploration or reduction, we just look at the
# resulting buckets.
{ bash, bucketCheck, ghcWithML4HSFE, mkBin, runCommand, withDeps, wrap,
  writeScript }:

with rec {
  haskellVersion = runCommand "recurrent-bucket"
    {
      buildInputs = [ ghcWithML4HSFE ];
      main        = ../haskell-support/RecurrentBucket.hs;
    }
    ''
      cp "$main" Main.hs
      ghc --make -o Main Main.hs
      mv Main "$out"
    '';

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
