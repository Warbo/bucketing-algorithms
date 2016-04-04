defs: with defs;

let deps = [
      explore-theories
      mlspec
      mlspec-bench
      haskellPackages.ArbitraryHaskell
      haskellPackages.ifcxt
      haskellPackages.mlspec
      haskellPackages.mlspec-bench
      haskellPackages.mlspec-helper
      haskellPackages.nix-eval
      haskellPackages.runtime-arbitrary
    ];
    pkg = buildEnv {
            name  = "testing-dependencies";
            paths = deps;
          };
    # Runs a shell script to check if the output directory of a derivation
    # exists. This will cause the derivation to be built, if it isn't already,
    # and hence will expose any errors in its build/install process.
    # We use this round-about method in preference to 'pathExists', since that
    # will complain about strings containing store paths depending on other
    # store paths ("cannot refer to other paths").
    usable = x: parseJSON (runScript {} ''
      set -e
      echo "Checking if package directory '${x}' exists" 1>&2
      if [[ -d "${x}" ]]
      then
        echo "true"  > "$out"
      else
        echo "false" > "$out"
      fi
    '');
in usable pkg && all usable deps
