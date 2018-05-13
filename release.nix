# A useful entry point for continuous integration (e.g. Hydra)
with rec {
  go = stable:
    with import ./. {
      args            = { inherit stable; };
      bypassPublicApi = true;
    };
    pkgs.stripOverrides {
      inherit package;
      benchmark = import ./benchmarks { inherit stable; };
    };
};
{
  stable   = go true;
  unstable = go false;
}
