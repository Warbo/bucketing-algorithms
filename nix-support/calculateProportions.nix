/*
Reads in JSON bucketing results, of the form:

  {
    size1 : {
      rep1 : [
        {
          "sampleNames" : [
            name1,
            ...
          ],
          "sampleTheorems" : [
            theorem1,
            ...
          ]
        },
        {
          method1 : {
            bucketCount1 : {
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
        }
      ],
      ...
    },
    ...
  }

For each rep of each size, the first array element contains all of the sampled
names and the full ground truth. The second element contains the results of a
bucketing method. Each bucketing method has entries for a variety of different
(target, or average) bucket counts; for example "1" will (attempt to) put all
names into one bucket, "5" will (attempt to) spread them across five buckets,
etc. Note that we say 'target', 'average' and 'attempt', since each method may
prefer to keep some names together even if other buckets are available.

For each size, the "names" entry shows how the names were divided into buckets
(conceptually this is a set of sets of names; the order is irrelevant). The
"theorems" entry shows the union of the ground truths of the buckets.

The purpose of this script is to calculate how much these 'unions of ground
truths of buckets' differ from the 'ground truth of the union of buckets' (i.e.
the full ground truth, if no bucketing were used).

The output of this script, when fed the above data, replaces each 'bucket size'
value with the ratio described above. We throw out the names and theorems, since
we may have a lot of data to deal with.

Note that some reps may be 'null', which indicates that their sample was a
duplicate of some other rep, and hence it's been discarded to prevent
double-conting.
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
        helper = ../haskell-support/MsgPack.hs;
        script = ../haskell-support/CalculateProportions.hs;
      }
      ''
        cp "$helper" MsgPack.hs
        cp "$script" Main.hs
        ghc --make -O2 -o "$out" Main.hs
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

      C=$(echo "$O" | jq '.["1"] | .["1"] | .[1] | .m | .["1"]') ||
        fail "Couldn't extract proportion from:\n$O"
      echo "$C" | jq -e '. == 0.3333333333333333' ||
        fail "Proportion didn't match expected:\n$C\nTaken from:\n$O"

      true
    '';
};
withDeps [ test ] cmd
