{ defaultVersion, fetchFromGitHub, stable }:

with rec {
  # This is pinned to a revision of nix-config which should work for everyone
  stableCfg = rec {
    nix-config     = import "${nix-config-src}" { inherit defaultVersion; };

    nix-config-src = fetchFromGitHub {
      owner  = "Warbo";
      repo   = "nix-config";
      rev    = "99bc878";
      sha256 = "0q8f30vzvngnnvszxxp6vhr649y4lvix4r9axhvmpc9wr5afls6s";
    };
  };

  # Try taking <nix-config> from the environment, if given, or else from git
  unstableCfg =
    with builtins.tryEval <nix-config>;
    if success
       then rec {
         nix-config     = import "${nix-config-src}" {
                            defaultVersion = "unstable";
                          };
         nix-config-src = value;
       }
       else rec {
         nix-config     = import <nixpkgs> {
                            config = stableCfg.nix-config.latestNixCfg;
                          };
         nix-config-src = nix-config.configSrc;
       };
};

# In the case that we're stable, or <nix-config> is provided, these act as a
# sanity check to ensure the APIs used by unstableCfg are still present
assert stableCfg.nix-config ? latestNixCfg ||
       abort "nix-config is missing latestNixCfg";
assert stableCfg.nix-config ? configSrc ||
       abort "nix-config is missing configSrc";

if stable then stableCfg else unstableCfg
