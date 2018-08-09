# A useful entry point for continuous integration (e.g. Hydra)
# Imports those useful derivations from default.nix, like user-facing packages,
# tests, benchmarks, etc.
{
  inherit (import ./.) benchmark package;
}
