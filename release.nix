# A useful entry point for continuous integration (e.g. Hydra)
with {
  go = stable:
    with import ./. {
      args            = { inherit stable; };
      bypassPublicApi = true;
    };
    {
      inherit package;
      benchmark   = if stable then import ./shell.nix else null;
      performance = pkgs.bucketProportions {
        maxSize = 10;
        reps    = 10;
      };
    };
};
{
  stable   = go true;
  unstable = go false;
}
