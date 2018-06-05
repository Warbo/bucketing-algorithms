{ buildEnv, hashBucket }:

{
  removeOverrides = true;
  value           = buildEnv {
    name  = "haskell-theory-exploration";
    paths = [ hashBucket ];
  };
}
