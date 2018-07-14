{ buildEnv, hashBucket }:

{
  def = buildEnv {
    name  = "haskell-theory-exploration";
    paths = [ hashBucket ];
  };
}
