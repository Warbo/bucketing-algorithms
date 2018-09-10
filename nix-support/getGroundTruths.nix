{ bash, bucketProportions, fail, ghcWithML4HSFE, haskellPackages, jq,
  makeSamples, mkBin, msgpack-tools, runCommand, tebenchmark, withDeps, wrap,
  writeScript }:

with rec {
  # Takes a "Main" file and compiles it, with some fixed dependencies
  buildMain = main: mkBin {
    name   = "buildMain";
    paths  = [ bash (ghcWithML4HSFE {}) ];
    vars   = {
      inherit main;
      helper1 = ../haskell-support/GetGroundTruthsHelpers.hs;
      helper2 = ../haskell-support/GetGroundTruths.hs;
      util    = ../haskell-support/BucketUtil.hs;
    };
    script = ''
      #!/usr/bin/env bash
      cp "$helper1" Helper.hs
      cp "$helper2" GetGroundTruths.hs
      cp "$util"    BucketUtil.hs
      cp "$main"    Main.hs
      ghc --make -O2 -o Main Main.hs
    '';
  };

  # Provides buildMain, plus the data required by our TemplateHaskell
  env = main: tebenchmark.cache // { buildInputs = [ (buildMain main) ]; };

  haskellTests = runCommand "get-ground-truths-tests"
    (env ../haskell-support/GetGroundTruthsTest.hs)
    ''
      buildMain
      ./Main || exit 1
      echo "Pass" > "$out"
    '';

  haskellVersion = runCommand "get-ground-truths-haskell"
    (env ../haskell-support/GetGroundTruthsMain.hs // { inherit haskellTests; })
    ''
      buildMain
      mv Main "$out"
    '';

  go = wrap {
    name = "get-ground-truths";
    file = haskellVersion;
    vars = {
      LANG   = "en_US.UTF-8";
      LC_ALL = "C";
    };
  };

  test = runCommand "test-get-ground-truths"
    {
      inherit go;
      inherit (bucketProportions) addBuckets;
      buildInputs = [ fail jq msgpack-tools ];

      # Manually simplified output of addBuckets. This triggered a bug in adding
      # ground truths, so we include it as a regression test.
      preBucketed =
        with {
          n1 = "global746970323031352f77656972645f6e61745f6d756c335f6173736f63312e736d74326d756c33";
          n2 = "global746970323031352f77656972645f6e61745f6f705f6173736f632e736d74326f70";
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

      smallSample = makeSamples { sizes = [ 2      ]; reps = 1; };
      midSample   = makeSamples { sizes = [ 1      ]; reps = 2; };
      bigSample   = makeSamples { sizes = [ 1 5 10 ]; reps = 3; };
    }
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
};
withDeps [ test ] go
