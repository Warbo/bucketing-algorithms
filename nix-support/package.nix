{ buildEnv, hashBucket }:

buildEnv {
  name  = "haskell-theory-exploration";
  paths = [
    hashBucket
  ];
}
