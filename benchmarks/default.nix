# Builds the environment in which to run a benchmark
args:

with builtins;
with import ../nix-support {};
with lib;
with rec {
  parameters = {
    max_size    = 20;
    repetitions = 1;
  };

  py = nixpkgs-2016-09.python3.withPackages (p: []);
};

mkBin {
  name  = "python";
  paths = [ py ];
  vars  = { parameters = toJSON parameters; };
  script = ''
    #!/usr/bin/env bash
    exec python3 "$@"
  '';
}
