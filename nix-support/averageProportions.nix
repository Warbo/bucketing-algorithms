{ calculateProportions, fail, jq, msgpack-tools, nixpkgs1803, runCommand,
  withDeps, wrap, writeScript }:

with builtins;
with rec {
  cmd = wrap {
    name  = "averageProportions";
    vars  = { LANG = "en_US.UTF-8"; };
    file  = runCommand "average-proportions"
      {
        buildInputs = [
          (nixpkgs1803.haskellPackages.ghcWithPackages (h: [
            h.bytestring h.containers h.data-msgpack h.text
          ]))
        ];
        main = ../haskell-support/AverageProportions.hs;
      }
      ''
        cp "$main" Main.hs
        ghc --make -O2 -o "$out" Main.hs
      '';
  };

  test = runCommand "test-averageProportions"
    {
      inherit calculateProportions cmd;
      buildInputs = [ fail jq msgpack-tools ];
      example     = writeScript "example.json" (toJSON {
        "1" = {
          "1" = [
            {
              sampleNames    = [    "n"    ];
              sampleTheorems = [ "t1" "t2" ];
            }
            {
              a = {
                "1" = {
                  names    = [ [ "n" ] ];
                  theorems = [   "t1"  ];
                };
                "2" = {
                  names    = [  [ "n" ]  ];
                  theorems = [ "t1" "t2" ];
                };
              };
              b = {
                "1" = {
                  names    = [ [ "n" ] ];
                  theorems = [         ];
                };
                "2" = {
                  names    = [ [ "n" ] ];
                  theorems = [   "t2"  ];
                };
              };
            }
          ];
          "2" = [
            {
              sampleNames    = [ "n2" ];
              sampleTheorems = [ "t3" ];
            }
            {
              a = {
                "1" = {
                  names    = [ [ "n2" ] ];
                  theorems = [   "t3"   ];
                };
                "2" = {
                  names    = [ [ "n2" ] ];
                  theorems = [   "t3"   ];
                };
              };
              b = {
                "1" = {
                  names    = [ [ "n2" ] ];
                  theorems = [          ];
                };
                "2" = {
                  names    = [ [ "n2" ] ];
                  theorems = [   "t3"   ];
                };
              };
            }
          ];
          "3" = null;
        };
      });
    }
    ''
      O=$(echo '{}' | json2msgpack | "$cmd") || fail "Failed on empty input\n$O"

      # NOTE: Do not try storing msgpack in Bash variables, since NULL bytes
      # will cause problems. Instead, we always convert to JSON, even if it
      # causes a bunch of round trips.
      PS=$(json2msgpack < "$example" |
           "$calculateProportions"   |
           msgpack2json) ||
        fail "Couldn't calculate proportions\n$PS"

      echo "$PS" | jq 'type' || {
        echo "$PS" | msgpack2json -d || true
        fail "Didn't calculate msgpack\n$PS"
      }

      O=$(echo "$PS" | json2msgpack | "$cmd" | msgpack2json) ||
        fail "Couldn't average. In:\n$PS\nOut:\n$O"

      echo "$O" | jq -e '.["1"] | keys | sort | . == ["a", "b"]' ||
        fail "Didn't have methods 'a' and 'b'\n$O"

      echo "$O" | jq -e 'map(map(keys | sort | . == ["1", "2"]) | all) | all' ||
        fail "Methods didn't have bucket sizes 1 and 2\n$O"

      echo "$O" | jq -e 'map(map(map(.proportion | keys | sort |
                                     . == ["mean", "stddev"]) |
                                 all) |
                             all) |
                         all' ||
        fail "Bucket sizes didn't have mean and stddev\n$O"

      WANT='{"a": {"1": 0.75, "2": 1   },
             "b": {"1": 0,    "2": 0.75}}'
      GOT=$(echo "$O" | jq -e '.[] | map_values(map_values(.mean))')
      echo "$GOT" | jq --argjson want "$WANT" '. == $want' ||
        fail "Expected means differ. Want:\n$WANT\nGot:\n$GOT\nOriginal:\n$O"

      mkdir "$out"
    '';
};
withDeps [ test ] cmd
