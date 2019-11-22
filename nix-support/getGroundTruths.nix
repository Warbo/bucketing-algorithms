{ attrsToDirs', bash, bucketProportions, composeBins, fail, ghcWithML4HSFE,
  haskellPackages, jq, lzip, makeSamples, mkBin, msgpack-tools, runCommand,
  runOn, tebenchmark, withDeps, wrap, writeScript }:

with rec {
  # Takes a "Main" file and compiles it, with some fixed dependencies
  buildMain = { deps, name, profile ? false, script, test ? false }:
    with rec {
      ghc  = ghcWithML4HSFE {
        inherit profile;
        extraPkgs = [ "bitset" "criterion" ];
      };

      code = attrsToDirs' "get-ground-truths-src" {
        "GetGroundTruths.hs" = ../haskell-support/GetGroundTruths.hs;
        "Helper.hs"          = ../haskell-support/GetGroundTruthsHelpers.hs;
        "BucketUtil.hs"      = ../haskell-support/BucketUtil.hs;
        "Main.hs"            = if test
          then ../haskell-support/GetGroundTruthsTest.hs
          else ../haskell-support/GetGroundTruthsMain.hs;
      };
    };
    runCommand "get-ground-truths-${name}"
      (tebenchmark.cache // deps // {
        inherit code;
        __noChroot  = true;
        buildInputs = [ ghc ];
      })
      ''
        cp "$code"/*.hs ./
        ${script}
      '';

  unitTests = buildMain {
    name   = "tests";
    deps   = {};
    test   = true;
    script = ''
      ghc --make -O2 -o Main Main.hs
      ./Main || exit 1
      echo "Pass" > "$out"
    '';
  };

  binary = wrap {
    name = "get-ground-truths";
    vars = { LANG = "en_US.UTF-8"; LC_ALL = "C"; };
    file = buildMain {
      name   = "get-ground-truths-without-env";
      deps   = { inherit unitTests; };
      script = ''
        ghc --make -O2 -o Main Main.hs
        mv Main "$out"
      '';
    };
  };

  # Inputs for testing and profiling; we want small ones to spot obvious errors,
  # bigger ones to exercise the code a bit more, and a more realistic size to
  # ensure profile information doesn't inflate overheads.
  testSamples = {
    smallSample = makeSamples { sizes = [ 2                 ]; reps = 1; };
    midSample   = makeSamples { sizes = [ 1                 ]; reps = 2; };
    bigSample   = makeSamples { sizes = [ 1 5  10           ]; reps = 3; };
    heftySample = makeSamples { sizes = [ 1 50 75 87 94 100 ]; reps = 5; };
  };

  integrationTests = runCommand "test-get-ground-truths"
    (testSamples // {
      inherit
        unitTests  # Only bother with these if the unit tests have passed
        ;
      inherit (bucketProportions) addBuckets;
      buildInputs = [ fail jq msgpack-tools ];
      go          = binary;

      # Manually simplified output of addBuckets. This triggered a bug in adding
      # ground truths, so we include it as a regression test.
      preBucketed =
        with {
          n1 = "global746970323031352f77656972645f6e61745f6d756c335f6173736f6" +
               "3312e736d74326d756c33";
          n2 = "global746970323031352f77656972645f6e61745f6f705f6173736f632e7" +
               "36d74326f70";
        };
        writeScript "ground-truth-regression-test.json" (builtins.toJSON {
          size1 = {
            rep1 = [
              { sampleNames =          [ n1 ];     }
              { recurrent   = { "1" = [[ n1 ]]; }; }
            ];
            rep2 = [
              { sampleNames =          [ n2 ];     }
              { recurrent   = { "1" = [[ n2 ]]; }; }
            ];
          };
        });
    })
    ''
      echo -e "\nTesting non-JSON" 1>&2
      O=$(echo 'nope' | "$go" 2>&1) && fail "Non-JSON should error\n$O"
      echo "Non-JSON passed" 1>&2

      echo -e "\nTesting empty JSON object" 1>&2
      O=$(echo '{}'   | "$go") || fail "JSON object should work\n$O"
      [[ "x$O" = "x{}" ]]    || fail "Object should give '{}', got '$O'"
      echo "Empty JSON object passed" 1>&2

      echo -e "\nTesting JSON array" 1>&2
      O=$(echo '[]'   | "$go" 2>&1) && fail "JSON array should fail\n$O"
      echo "JSON array passed" 1>&2

      echo -e "\nTesting JSON null" 1>&2
      O=$(echo 'null' | "$go" 2>&1) && fail "JSON null should fail\n$O"
      echo "JSON null passed" 1>&2

      echo -e "\nTesting non-empty object" 1>&2
      O=$(echo '{"1":{"2":[{"sampleNames":[]},{}]}}' | "$go") ||
        fail "Non-empty object failed\n$O"
      WANT='{"1":{"2":[{"sampleNames":[],"sampleTheorems":[]},{}]}}'
      echo "$O" | jq -e --argjson want "$WANT" '. == $want' ||
        fail "Non-empty object should produce '$WANT', got '$O'"
      unset WANT

      echo -e "\nTesting non-empty sample" 1>&2
      O=$(echo '{"1":{"2":[{"sampleNames":["global64"]},{}]}}' | "$go") ||
        fail "Non-empty sample failed"
      echo "$O" | jq -e '.["1"] | .["2"] | length | . == 2' ||
        fail "Non-empty sample should get 1 entry for sample and 1 for methods"
      echo "$O" | jq -e '.["1"] | .["2"] | .[0] | type | . == "object"' ||
        fail "Non-empty sample should become an object, got '$O'"
      echo "$O" | jq -e '.["1"] | .["2"] | .[0] | has("sampleNames")' ||
        fail "Non-empty sample should get 'sampleNames', got '$O'"
      echo "$O" | jq -e '.["1"] | .["2"] | .[0] | has("sampleTheorems")' ||
        fail "Non-empty sample should have 'sampleTheorems', got '$O'"
      echo "$O" | jq -e '.["1"] | .["2"] | .[0] | .sampleNames |
                                  . == ["global64"]' ||
        fail "'sampleNames' should match input '[\"global64\"]', got '$O'"
      echo "$O" | jq -e '.["1"] | .["2"] | .[0] | .sampleTheorems | . == []' ||
        fail "Bogus names should get empty 'sampleTheorems', got '$O'"
      echo "Non-empty sample passed" 1>&2

      echo -e "\nChecking if regression test causes crash" 1>&2
      "$go" < "$preBucketed" | jq 'type' || fail "Regression test failed"

      echo -e "\nTesting small sample" 1>&2
      "$addBuckets" < "$smallSample" > smallBucketed ||
        fail "Couldn't bucket small sample"

      O=$("$go" < smallBucketed) || fail "Didn't get small ground truths"

      echo "$O" | jq -e 'map(map(.[0] | .sampleTheorems
                                      | length
                                      | . > 0) | all) | all' ||
        fail "Small samples should have at least one theorem '$O'"
      echo "Small sample passed" 1>&2

      echo "Testing larger sample" 1>&2
      "$addBuckets" < "$midSample" > midBucketed

      O=$("$go" < midBucketed) || fail "Didn't get mid ground truths"

      echo -e "\nTesting larger sample" 1>&2
      "$addBuckets" < "$bigSample" > bucketed ||
        fail "Couldn't bucket big sample"

      O=$("$go" < bucketed) || fail "Didn't get ground truths of big sample"
      echo "$O" | jq -e 'map(map(.[0] | .sampleTheorems
                                      | length
                                      | . > 0) | all) | all' ||
        fail "Every sample should have at least one theorem '$O'"
      echo "Larger sample passed" 1>&2

      mkdir "$out"
    '';

  # Build for profiling, and profile time usage on a moderately sized input
  profiled = buildMain {
    name    = "profiled";
    deps    = {
      inherit integrationTests;
      samples = runOn "hefty-sample-with-buckets.lz"
        (composeBins "add-buckets-lzip" [
          bucketProportions.addBuckets
          "${lzip}/bin/lzip"
        ])
        testSamples.heftySample;
    };
    profile = true;
    script  = ''
      DIR="$PWD"

      # TemplateHaskell requires dynamic linking, which complicates profiling.
      # We need to invoke GHC multiple times; these options came from
      # https://github.com/haskell/cabal/issues/1553#issuecomment-36515415
      function go { ghc --make -O2 "$@" -o Main Main.hs; }

      go
      go -prof -osuf p_o -fprof-auto

      mkdir "$out"
      cd    "$out"
      "${lzip}/bin/lzip" -d < "$samples" | "$DIR"/Main +RTS -p > /dev/null
    '';
  };

};
withDeps [ profiled ] binary
