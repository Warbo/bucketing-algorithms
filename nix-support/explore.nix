{ allDrvsIn, checkHsEnv, coreutils, fail, haskellPackages, jq, lib, mkBin, nix,
  nixEnv, pkgName, runCommand, sanitiseName, timeout, withDeps, wrap,
  writeScript }:
with builtins;
with lib;
with rec {

explore-theories = withDeps
  (explore-no-dupes ++ allDrvsIn explore-finds-equations ++ [
    explore-exit-success
  ])
  explore-theories-untested;

explore-theories-untested = mkBin {
  name   = "explore-theories";
  paths  = [ coreutils ];
  vars   = {
    runner = wrap {
      name   = "explore-runner";
      paths  = [ fail jq timeout haskellPackages.mlspec nix ];
      vars   = nixEnv;
      script = ''
        #!/usr/bin/env bash
        set -e
        set -o pipefail

        function noDepth {
          grep -v "^Depth" || true # Don't abort if nothing found
        }

        function checkForErrors {
          ERR=0
          while read -r LINE
          do
            echo "$LINE"
            if echo "$LINE" | grep "No instance for" > /dev/null
            then
              ERR=1
            fi
          done
          [[ "$ERR" -eq 0 ]] || fail "Haskell error, aborting"
        }

        # limit time/memory
        withTimeout MLSpec "$@" 2> >(checkForErrors 1>&2) | noDepth | jq -s '.'
      '';
    };
  };
  script = ''
    #!/usr/bin/env bash
    set -e
    if [[ -n "$MAX_SECS" ]]
    then
      timeout "$MAX_SECS" "$runner" "$@"
    else
      "$runner" "$@"
    fi
  '';
};

hsPkgsInEnv = env: names: runCommand "checkIfHsPkgsInEnv" env ''
  set -e
  "${checkHsEnv names}"
  echo "true" > "$out"
'';

# Make a list of packages suitable for a 'buildInputs' field. We treat Haskell
# packages separately from everything else. The Haskell packages will include:
#
#  - The contents of 'extra-haskell-packages', required for our scripts
#  - The contents of the 'extraHs' argument; use '[]' for none
#  - The cabal package contained at path 'standalone' (requires a 'default.nix'
#    file, e.g. from 'nixFromCabal'); use 'null' for none
#  - Any 'package' fields read from the JSON file 'f'
#
# The non-Haskell packages will include:
#
#  - The contents of extra-packages, required by our scripts
#  - The contents of the 'extraPkgs' argument; use '[]' for none
#
extractedEnv = { extraPkgs ? [], extraHs ? [], standalone ? null, f ? null }:
  with rec {
    names     = if standalone == null then [] else [ name ];
    pkgs      = h: if standalone == null || elem name hsNames
                      then []
                      else [ (pkg h) ];
    name      = assert standalone != null; pkgName (pkg haskellPackages).name;
    pkg       = assert standalone != null; h: h.callPackage (import standalone) {};
    extras    = extra-packages ++ extraPkgs;
    hsNames   = unique (map pkgName (extra-haskell-packages ++ extraHs));
    ghcEnv    = haskellPackages.ghcWithPackages (h: pkgs h ++ (concatMap
                  (n: if hasAttr n haskellPackages
                         then [ (getAttr n h) ]
                         else [])
                  hsNames));
    check     = hsPkgsInEnv { buildInputs = [ ghcEnv ] ++ extras; }
                            (hsNames ++ names);
  };
  [ (withDeps [ check ] ghcEnv) ] ++ extras;

# Haskell packages required for MLSpec
extra-haskell-packages = [ "mlspec" "mlspec-helper" "runtime-arbitrary"
                           "quickspec" "QuickCheck" "AstPlugin" ];

extra-packages = [ jq ];

exploreEnv = extra-packages ++ [
  (haskellPackages.ghcWithPackages (h: map (n: getAttr n h)
                                           extra-haskell-packages))
];

explore-exit-success =
  with { f = ../tests/exploreTheoriesExamples/hastily.formatted.1; };
  runCommand "exploreExitSuccess"
    {
      inherit f;
      buildInputs = extractedEnv { inherit f; } ++
                    [ explore-theories-untested fail ];
    }
    ''
      set -e

      OUTPUT=$(explore-theories "$f" 2>&1 | tee >(cat 1>&2)) ||
        fail "Failed to explore 'hastily' ($OUTPUT)"

      mkdir "$out"
    '';

explore-finds-equations =
  with rec {
    path           = ../tests/exploreTheoriesExamples;
    files          = mapAttrs (f: _: "${path}/${f}") (readDir path);
    foundEquations = name: f: runCommand "explore-test-${name}"
      {
        buildInputs = extractedEnv { inherit f; } ++
                      [ explore-theories-untested fail ];
        inherit f;
      }
      ''
        set -e
        set -o pipefail

        function finish {
          if [[ -e sout ]]
          then
            echo -e "\n\nstdout:\n\n" 1>&2
            cat sout                  1>&2
          else
            echo "No stdout produced" 1>&2
          fi

          if [[ -e serr ]]
          then
            echo -e "\n\nstderr:\n\n" 1>&2
            cat serr                  1>&2
          else
            echo "No stderr produced" 1>&2
          fi

          echo "COUNT: $COUNT" 1>&2
        }
        trap finish EXIT

        echo "Exploring '$f'" 1>&2
        RESULT=$(explore-theories < "$f" 2> serr | tee >(cat 1>&2)) ||
          fail "Failed to explore '$f'"

        if grep "No clusters found" < serr
        then
          fail "No clusters found by MLSpec (did it receive any input?)"
        fi

        COUNT=$(echo "$RESULT" | jq 'length')

        if [[ "$COUNT" -eq 0 ]]
        then
          echo -e "Couldn't find any equations in output of '$f'" 1>&2
        else
          echo "Found '$COUNT' equations for '$f'" 1>&2
        fi

        mkdir "$out"
      '';
  };
  mapAttrs foundEquations files;

explore-no-dupes =
  with rec {
    path       = toString ../tests/exploreTheoriesExamples;
    files      = map (f: "${path}/${f}") (attrNames (readDir path));
    noDupesFor = f: runCommand "no-dupes-for-${sanitiseName (baseNameOf f)}"
      {
        inherit f;
        buildInputs = extractedEnv {} ++ [ fail explore-theories-untested ];
      }
      ''
        set -e
        set -o pipefail

        function noDupes {
          echo "Removing dupes" 1>&2
          DUPES=$(grep "^building path.*repo-head" |
                  sed -e 's/.*head-//g'            |
                  sort                             |
                  uniq -D) || DUPES=""
          [[ -z "$DUPES" ]] || {
            echo "Made redundant package lookups: $DUPES" 1>&2
            exit 1
          }
        }

        echo "Exploring '$f'" 1>&2
        OUTPUT=$(explore-theories < "$f" 2>&1 | tee >(cat 1>&2)) ||
          fail "Failed to explore '$f'\nOUTPUT:\n\n$OUTPUT\n\n"

        echo "$OUTPUT" | noDupes
        mkdir "$out"
      '';
  };
  map noDupesFor files;
};
{
  inherit extra-haskell-packages explore-theories exploreEnv
          extractedEnv;
}
