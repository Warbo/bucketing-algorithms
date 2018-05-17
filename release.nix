# A useful entry point for continuous integration (e.g. Hydra)
with rec {
  go = stable:
    with import ./. {
      args            = { inherit stable; };
      bypassPublicApi = true;
    };
    pkgs.stripOverrides
      ({ inherit package; } // (if stable
                                   then { benchmark = import ./shell.nix; }
                                   else {}));
};
{
  stable   = go true;
  unstable = go false;
}
