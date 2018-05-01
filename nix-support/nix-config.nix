{ fetchFromGitHub, stable }:

with rec {
  # This is pinned to a revision of nix-config which should work for everyone
  stableCfg = rec {
    nix-config     = import "${nix-config-src}" {};

    nix-config-src = fetchFromGitHub {
      owner  = "Warbo";
      repo   = "nix-config";
      rev    = "1babef1";
      sha256 = "1p0kn70l9a9i8r7318k1w51ncajcvw3vk0yln6b6r3yhfcdp51by";
    };
  };

  # Try taking <nix-config> from the environment, if given, or else from git
  mkUnstableCfg =
    with builtins.tryEval <nix-config>;
    if success
       then rec {
         nix-config     = import "${nix-config-src}" { stable = false; };
         nix-config-src = value;
       }
       else rec {
         nix-config     = stableCfg.nix-config.latestNixCfg;
         nix-config-src = nix-config.configSrc;
       };
};

# In the case that we're stable, or <nix-config> is provided, these act as a
# sanity check to ensure the APIs used by mkUnstableCfg are still present
assert stableCfg.nix-config ? latestNixCfg ||
       abort "nix-config is missing latestNixCfg";
assert stableCfg.nix-config ? configSrc ||
       abort "nix-config is missing configSrc";

if stable then stableCfg else unstableCfg
