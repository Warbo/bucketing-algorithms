# A useful entry point for continuous integration (e.g. Hydra)
with import ./. {
  args            = {};
  bypassPublicApi = true;
};
{
  inherit package;
  benchmark   = import ./shell.nix;
  performance = pkgs.bucketProportions {
    maxSize = 10;
    reps    = 10;
  };
}
