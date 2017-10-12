{ annotated, bash, buckets, buildEnv, cluster, explore, fail, format,
  glibcLocales, hashspecBench, jq, lib, mkBin, nix-config, reduce-equations,
  runCommand, runWeka, stdenv, timeout, tipBenchmarks, withDeps, writeScript }:
with builtins;
with lib;
with {
  inherit (nix-config) wrap;
};
rec {

  benchVars = {
    sampled = {
      inherit (hashspecBench.benchVars.sampled) genInput;

      runner  = wrap {
        name  = "mlspec-sampled-runner";
        paths = [ ((import hashspecBench.augmentedHs {
          hsDir = "${tipBenchmarks.tip-benchmark-haskell}";
        }).ghcWithPackages (h: map (n: h."${n}") [
          "quickspec" "murmur-hash" "cereal" "mlspec-helper"
          "tip-benchmark-sig" "runtime-arbitrary" "QuickCheck" "ifcxt"
          "hashable" "mlspec"
        ]))

        reduce-equations
        buckets.hashes
        runWeka ];
        script = inEnvScript;
      };
    };
  };

  ourEnv = writeScript "our-env.nix" ''
    with import ${./..}/nix-support {};
    buildEnv {
      name  = "mlspecbench-env";
      paths = [
        ((import ${hashspecBench.customHs}).ghcWithPackages (h: [
          h.tip-benchmark-sig h.mlspec
        ]))
        runWeka
        jq
      ];
    }
  '';

  assertNumeric = var: msg:
    assert hasPrefix "$" var || abort (toString {
      function = "assertNumeric";
      error    = "Argument 'var' should be bash variable with '$'";
      inherit var msg;
    });
    ''
      echo "${var}" | grep -o "^[0-9][0-9]*\$" > /dev/null || {
        echo 'Error, ${var}' "(${var})" 'is not numeric: ${msg}' 1>&2
        exit 1
      }
    '';

  inEnvScript = wrap {
    name   = "mlspecBench-inenvscript";
    paths  = [ bash explore.explore-theories jq reduce-equations timeout ];
    vars   = {
      NIX_EVAL_EXTRA_IMPORTS = ''[("tip-benchmark-sig", "A")]'';
      SIMPLE                 = "1";
    };
    script =  ''
      #!/usr/bin/env bash
      set -e
      set -o pipefail

      # Perform clustering
      CL=$(${cluster})

      clCount=$(echo "$CL" | jq 'map(.cluster) | max')
      ${assertNumeric "$clCount" "clCount should contain number of clusters"}

      export clCount

      if [[ -n "$EXPLORATION_MEM" ]]
      then
        echo "Limiting memory to '$EXPLORATION_MEM'" 1>&2
        export MAX_KB="$EXPLORATION_MEM"
      fi

      echo "$CL" | "${format.fromStdin}" |
        withTimeout explore-theories | reduce-equations
    '';
  };

  mlGenInput = wrap {
    name   = "gen-input";
    paths  = [ bash jq tipBenchmarks.tools ];
    vars   = {
      OUT_DIR   = tipBenchmarks.tip-benchmark-haskell;
      ANNOTATED = annotated {
        pkgDir = toString tipBenchmarks.tip-benchmark-haskell;
      };
      filter = writeScript "filter.jq" ''
        def mkId: {"name": .name, "package": .package, "module": .module};

        def keep($id): $keepers | map(. == $id) | any;

        def setQS: . + {"quickspecable": (.quickspecable and keep(mkId))};

        map(setQS)
      '';
    };
    script = ''
      #!/usr/bin/env bash
      set -e
      set -o pipefail

      # Sample some names, give the default module and package, then slurp
      # into an array
      echo "Running 'choose_sample $1 $2'" 1>&2
      KEEPERS=$(choose_sample "$1" "$2" |
                jq -R '{"name"    : .,
                        "module"  : "A",
                        "package" : "tip-benchmark-sig"}' |
                jq -s '.')

      # Filters the signature to only those sampled in KEEPERS
      jq --argjson keepers "$KEEPERS" -f "$filter" < "$ANNOTATED" |
        jq 'map(select(.quickspecable))'
    '';
  };

  mlAllInput = wrap {
    name   = "all-input";
    paths  = [ fail ];
    script = ''
      #!/usr/bin/env bash
      set -e

      [[ -n "$ANNOTATED" ]] || fail "Got no ANNOTATED"

      ANN_F=$(readlink -f ANNOTATED)
      [[ -f "$ANN_F" ]] || fail "Annotated '$ANNOTATED' isn't a file (or link)"

      [[ -n "$OUT_DIR" ]] || fail "Got no OUT_DIR"

      OUT_D=$(readlink -f "$OUT_DIR")
      [[ -d "$OUT_D" ]] || fail "OUT_DIR '$OUT_DIR' isn't a dir (or link)"

      cat "$ANNOTATED"
    '';
  };

  rawScript = writeScript "mlspecBench" ''
    #!${bash}/bin/bash
    set -e

    ${hashspecBench.setUpDir}
    export TEMPDIR="$DIR"
    ${hashspecBench.getInput}

    # Explore
    export    NIXENV="import ${ourEnv}"
    export       CMD="${inEnvScript}"

    if [[ -n "$SAMPLE_SIZES" ]]
    then
      echo "Looping through sample sizes" 1>&2
      for SAMPLE_SIZE in $SAMPLE_SIZES
      do
        echo "Limiting to a sample size of '$SAMPLE_SIZE'" 1>&2
        export GEN_INPUT="${mlGenInput}"
        INFO="$SAMPLE_SIZE" benchmark
      done
    else
      echo "No sample size given, using whole signature" 1>&2
      export GEN_INPUT="${mlAllInput}"
      INFO="" benchmark
    fi
  '';

  mls-untested = mkBin {
    name = "mlspecBench";
    file = hashspecBench.wrapScript "mlspecBench" rawScript;

  };

  MAX_SECS = "300";
  testFile = name: path: runCommand "mls-${name}"
    {
      inherit MAX_SECS;
      buildInputs = [ fail jq mls-untested ];
    }
    ''
      echo "Running ${name} through mlspecBench" 1>&2
      OUTPUT=$(mlspecBench < "${path}") ||
        fail "Couldn't explore ${name}"

      T=$(echo "$OUTPUT" |
          jq 'has("cmd") and has("info") and has("results")') ||
        fail "Couldn't parse output\nSTART\n$OUTPUT\nEND"

      [[ "x$T" = "xtrue" ]] ||
        fail "Required fields missing:\n$OUTPUT"
      mkdir "$out"
    '';

  mls = withDeps [
    (testFile "list-full"  ../benchmarks/list-full.smt2)
    (testFile "nat-full"   ../benchmarks/nat-full.smt2)
    (testFile "nat-simple" ../benchmarks/nat-simple.smt2)
    (attrValues (mapAttrs (name: runCommand name {
                  inherit MAX_SECS;
                  buildInputs = [ fail jq mls-untested tipBenchmarks.tools ];
                }) {
      canRun = ''
        set -e
        mlspecBench < "${../tests/example.smt2}"
        mkdir "$out"
      '';

      outputIsJson = ''
        set -e
        OUTPUT=$(mlspecBench < ${../tests/example.smt2}) || exit 1
        TYPE=$(echo "$OUTPUT" | jq -r 'type') || {
          echo -e "START OUTPUT\n$OUTPUT\nEND OUTPUT" 1>&2
          exit 1
        }
        [[ "x$TYPE" = "xobject" ]] || {
          echo -e "START OUTPUT\n$OUTPUT\nEND OUTPUT" 1>&2
          echo "Type is '$TYPE' instead of object" 1>&2
          exit 1
        }
      '';

      haveEquations = ''
        set -e
        OUTPUT=$(mlspecBench < ${../tests/example.smt2})   || exit 1
         CHECK=$(echo "$OUTPUT" | jq 'has("results")') || exit 1
        [[ "x$CHECK" = "xtrue" ]] || {
          echo -e "Didn't find 'results' in\n$OUTPUT" 1>&2
          exit 1
        }
      '';

      filterSamples =
        with {
          keepers = map (name: {
                          inherit name;
                          module  = "A";
                          package = "tip-benchmark-sig";
                        }) [ "append" "constructorNil" "constructorCons" ];
        };
        ''
          set -e

          BENCH_OUT=$(CLUSTERS=1 SAMPLE_SIZES="5" mlspecBench)

          # Get all the constant symbols in all equations
          STDOUTS=$(echo "$BENCH_OUT" | jq -r '.results | .[] | .stdout') ||
            fail "Couldn't get stdouts\n\n$BENCH_OUT"

          OUTPUTS=$(while read -r F
                    do
                      cat "$F"
                    done < <(echo "$STDOUTS")) ||
            fail "Couldn't concat stdouts\n\n$BENCH_OUT\n\n$STDOUTS"

          EQS=$(echo "$OUTPUTS" | grep -v '^Depth') ||
            fail "Couldn't get eqs\n\n$BENCH_OUT\n\n$OUTPUTS"

          NAMES=$(echo "$EQS" |
                  jq -r 'getpath(paths(type == "object" and .role == "constant")) |
                         .symbol' |
                  sort -u) || fail "Couldn't get names\n\n$BENCH_OUT\n\n$EQS"
          SAMPLE=$(choose_sample 5 1)

          # Remove any names which appear in the sample
          while read -r NAME
          do
            NAMES=$(echo "$NAMES" | grep -vFx "$NAME") || true
          done < <(echo "$SAMPLE")

          # If there are any names remaining, they weren't in the sample
          if echo "$NAMES" | grep '^.' > /dev/null
          then
            DBG="NAMES:\n$NAMES\n\nOUTPUT:\n$BENCH_OUT\nSAMPLE:\n$SAMPLE"
            fail "Found names which aren't in sample\n$DBG"
          fi

          mkdir "$out"
        '';
    }))
  ] mls-untested;
}
