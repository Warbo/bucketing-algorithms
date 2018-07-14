{ callHackage, haskellSources, lib }:

with builtins;
with lib;
{
  def = helf: huper:
    mapAttrs (_: p: trace (toJSON { inherit p; }) helf.callPackage p {})
             haskellSources;
}
