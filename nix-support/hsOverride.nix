{ callHackage, haskellSources, lib }:

with builtins;
with lib;
{
  def = helf: huper:
    mapAttrs (_: p: helf.callPackage p {})
             haskellSources;
}
