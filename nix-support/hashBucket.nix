# Command to read annotated ASTs from stdin, and write them to stdout grouped
# into "buckets". Chooses what to put in each bucket arbitrarily by using a
# hash of the names.
{ bucketCheck, fail, haskellPackages, jq, mkBin, runCommand, withDeps,
  writeScript }:
with rec {
  hashBucket = runCommand "hashBucket"
    {
      buildInputs = [ (haskellPackages.ghcWithPackages (h: [
        h.aeson h.cryptonite h.memory h.unordered-containers
      ])) ];
      main = ../haskell-support/HashBucket.hs;
    }
    ''
      cp "$main" Main.hs
      ghc --make Main.hs -o "$out"
    '';

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
