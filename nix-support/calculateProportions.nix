/*
Reads in JSON bucketing results, of the form:

  {
    size1 : {
      rep1 : {
        "sample" : {
          "names" : [
            name1,
            ...
          ],
          "theorems" : [
            theorem1,
            ...
          ]
        },
        method1 : {
          bucketSize1 : {
            "names" : [
              [
                name1,
                ...
              ],
              ...
            ],
            "theorems" : [
              theorem1,
              ...
            ]
          },
          ...
        },
        ...
      },
      ...
    },
    ...
  }

For each rep of each size, the "sample" entry contains all of the sampled names
and the full ground truth. Each sibling of "sample" contains the results of a
bucketing method. Each bucketing method has entries for a variety of different
(target, or average) bucket sizes; for example "1" will (attempt to) put each
name in its own bucket, "5" will (attempt to) put five names in each bucket,
etc. Note that we say 'target', 'average' and 'try to', since each method may
prefer to keep some names together even if other buckets are available.

For each size the "names" entry shows how the names were divided into buckets
(conceptually this is a set of sets of names; the order is irrelevant). The
"theorems" entry shows the union of the ground truths of the buckets.

The purpose of this script is to calculate how much these 'unions of ground
truths of buckets' differ from the 'ground truth of the union of buckets' (i.e.
the full ground truth, if no bucketing were used).
*/
{ fail, jq, msgpack-tools, nixpkgs1803, python3, runCommand, withDeps, wrap }:

with builtins;
with rec {
  cmd = wrap {
    name = "calculateProportions";
    vars = { LANG = "en_US.UTF-8"; };
    file = runCommand "calculateProportions"
      {
        buildInputs = [
          (nixpkgs1803.haskellPackages.ghcWithPackages (h: [
            h.bytestring h.data-msgpack h.text
          ]))
        ];
        script = ../haskell-support/CalculateProportions.hs;
      }
      ''
        cp "$script" Main.hs
        ghc --make -o "$out" Main.hs
      '';
  };

  test = runCommand "test-calculateProportions"
    {
      inherit cmd;
      buildInputs = [ fail jq msgpack-tools ];

      noSample = toJSON {
        "1" = {
          "1" = [
            {
              foo = {
                "1" = {
                  names    = [ "x" ];
                  theorems = [ "y" ];
                };
              };
            }
          ];
        };
      };

      nameError = toJSON {
        "1" = {
          "1" = [
            {
              sampleNames    = [ "n" ];
              sampleTheorems = [ "t" ];
            }
            {
              m = {
                "1" = {
                  names    = [ ["n"] ["m"] ];
                  theorems = [             ];
                };
              };
            }
          ];
        };
      };

      oneThird = toJSON {
        "1" = {
          "1" = [
            {
              sampleNames    = [     "n"     ];
              sampleTheorems = [ "t" "u" "v" ];
            }
            {
              m = {
                "1" = {
                  names    = [ [ "n" ] ];
                  theorems = [   "u"   ];
                };
              };
            }
          ];
          "2" = null;
        };
      };
    }
    ''
      set -o pipefail
      mkdir "$out"

      function json {
        echo "$*" | json2msgpack
      }

      function go {
        "$cmd" "$@" | msgpack2json
      }

      O=$(json '{}' | go | tee "$out/empty.json") ||
        fail "Failed on empty object\n$O"
      echo "$O" | jq -e '. == {}' || fail "Wanted {}, got:\n$O"

      O=$(json "$noSample" | go 2>&1 | tee "$out/nosample.json") &&
        fail "Should've failed without 'sample'\n$O"

      O=$(json "$nameError" | go 2>&1 | tee "$out/nameerror.json") &&
        fail "Should've failed with differing names\n$O"

      O=$(json "$oneThird" | go | tee "$out/onethird.json") ||
        fail "Failed when given valid input\n$O"

      C=$(echo "$O" | jq '.["1"] | .["1"] | .[1] | .m | .["1"] | .comparison') ||
        fail "Couldn't extract comparison data from:\n$O"
      echo "$C" | jq -e '. == {"found"     :1,
                               "available" :3,
                               "missing"   :2,
                               "proportion":0.3333333333333333}' ||
        fail "Comparison data didn't match expected:\n$C\nTaken from:\n$O"

      true
    '';
};
withDeps [ test ] cmd
