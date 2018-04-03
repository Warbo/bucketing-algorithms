{ fetchFromGitHub, path, stable }:

with rec {
  config    = import "${configSrc}";

  configSrc = with builtins.tryEval <nix-config>;
              if success then value else stableSrc;

  stableSrc = fetchFromGitHub {
    owner  = "Warbo";
    repo   = "nix-config";
    rev    = "4b86cd3";
    sha256 = "1a7ji3y97xh963zhw2q7a7z62xr1zc5alp8k2835rqlkkq8h8zrx";
  };

  unstableSrc = (config { unstablePath = path; }).latestGit {
    url    = http://chriswarbo.net/git/nix-config.git;
    stable = { unsafeSkip = true; };
  };
};

{
  nix-config = if stable
                  then config
                  else import "${unstableSrc}";
}
