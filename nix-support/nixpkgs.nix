# Provides a fresh, stable revision of nixpkgs to use instead of <nixpkgs>
with builtins;

# We use fetchFromGitHub to get nixpkgs, but where do we get fetchFromGitHub
# from if we don't have nixpkgs? It depends. Usually, we get fetchFromGitHub
# from whichever <nixpkgs> happens to be in use (most likely the normal,
# system-wide set of packages). This doesn't introduce any new dependencies or
# impurities, since we're fetching a "fixed output derivation" (i.e. we've
# specified the result's sha256); assuming nobody's overridden fetchFromGitHub
# to ignore it, we'll either get the desired revision of nixpkgs every time, or
# we'll get an error (e.g. if the GitHub repo's been deleted).

# However, if <nixpkgs> has already been replaced with our git clone (e.g. if
# Nix has been called by one of our build scripts) then we can't use its
# version of fetchFromGitHub, since that would create a circular dependency.
# Instead, when calling Nix recursively (via 'withNix'), we store the original
# <nixpkgs> path as the path <real>, and use that to get fetchFromGitHub.

let

# If we have a <real> path, use that as the source of fetchFromGitHub. Otherwise
# use <nixpkgs>
path    = with tryEval <real>;
          if success then value else <nixpkgs>;

fetch   = rev: sha256: import ((import path {}).fetchFromGitHub {
            inherit rev sha256;
            owner = "NixOS";
            repo  = "nixpkgs";
          });

in args: {
  nixpkgs-2016-03 = fetch
    "16.03"
    "0m2b5ignccc5i5cyydhgcgbyl8bqip4dz32gw0c6761pd4kgw56v"
    args;

  nixpkgs-2016-09 = fetch
    "16.09"
    "1cx5cfsp4iiwq8921c15chn1mhjgzydvhdcmrvjmqzinxyz71bzh"
    args;
}
