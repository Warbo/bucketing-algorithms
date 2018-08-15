# A useful entry point for continuous integration (e.g. Hydra)
# Imports those useful derivations from default.nix, like user-facing packages,
# tests, benchmarks, etc.
with import ./.;
{
  inherit benchmark package;
  proportionExperiment = { inherit (proportionExperiment) test; };
}
