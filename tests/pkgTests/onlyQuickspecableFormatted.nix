defs: with defs; pkg:
with builtins;
with lib;

let

check = data:
          stdenv.mkDerivation {
            name         = "only-quickspecable-formatted";
            buildInputs  = [ jq ];
            msg          = "Ensuring all quickspecable ${toJSON data}";
            passAsFile   = [ "msg" ];
            buildCommand = ''
              source $stdenv/setup

              set -e
              STDOUT=$(jq 'map(.quickspecable) | all' < "${data}")

              if [[ -z "$STDOUT" ]]
              then
                echo "Got empty output"                  1>&2
                printf "not ok - %s" "$(cat "$msgPath")" 1>&2
                exit 1
              fi

              if [[ "x$STDOUT" = "xtrue" ]]
              then
                printf "ok - %s" "$(cat "$msgPath")" 1>&2
                touch "$out"
                exit 0
              else
                echo "not ok - %s" "$(cat "$msgPath")" 1>&2
                exit 1
              fi
            '';
          };

checkAll = mapAttrs (c: fmt: testWrap (map check fmt)
                                      "Checking ${c} quickspecable")
                    pkg.formatted;

in testWrap (map (c: checkAll."${toString c}") defaultClusters)
            "Only 'quickspecable' definitions get formatted for ${pkg.name}"
