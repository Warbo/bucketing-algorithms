# Checks the input/output behaviour of a given bucketing command, e.g. making
# sure that the correct JSON structure is used, etc.
{ fail, jq, runCommand }:

{ name, cmd, go }: runCommand "bucket-check-${name}"
  {
    inherit go;
    buildInputs = [ fail cmd jq ];
  }
  ''
    set -e
    set -o pipefail

    echo "Testing empty input" 1>&2
    echo "" | CLUSTER_SIZE=10 "$go" | jq -e 'length | . == 0'

    function mkObj {
      echo '{
        "package"       : "p",
        "module"        : "M",
        "name"          : "'"$1"'",
        "type"          : "'"$2"'",
        "ast"           : "(\"Lit\" (\"LitInteger\" 42))",
        "dependencies"  : [],
        "quickspecable" : true
      }'
    }

    echo "Testing single input" 1>&2
    O=$(mkObj foo T)
    GOT=$(echo "[$O]" | CLUSTER_SIZE=10 "$go")
    echo "$GOT" | jq -e --argjson o "$O" '. == [[{"package" : $o.package,
                                                  "module"  : $o.module ,
                                                  "name"    : $o.name   }]]' ||
      fail "Expected '[[$O]]' with only name/module/package; got '$GOT'"

    O=$(echo "[$(mkObj foo T), $(mkObj bar U)]" |
          CLUSTER_SIZE=1 "$go") || fail "Didn't bucket"
    echo "$O" | jq -e 'type | . == "array"' ||
      fail "Wrong result type\n$O"
    echo "$O" | jq -e 'length | . <= 2' ||
      fail "Wrong number of buckets\n$O"
    echo "$O" | jq -e 'map(type | . == "array") | all' ||
      fail "Wrong bucket types\n$O"
    echo "$O" | jq -e 'map(length) | add | . == 2' ||
      fail "Wrong bucket lengths\n$O"
    echo "$O" | jq -e 'map(.[] | .name) | sort | . == ["bar", "foo"]' ||
      fail "Wrong names\n$O"

    mkdir "$out"
  ''
