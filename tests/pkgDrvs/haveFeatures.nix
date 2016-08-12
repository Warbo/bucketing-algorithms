defs: with defs; pkg:
with builtins;

drvFromScript { buildInputs = [ ML4HSFE ];
                inherit (pkg) features annotated; } ''
  set -e
  echo "$info" 1>&2

  ANNCOUNT=$(jq 'length' < "$annotated")
  echo "ANNCOUNT: $ANNCOUNT" 1>&2

  COUNT=$(jq 'length' < "$features")

  echo "COUNT: $COUNT" 1>&2

  if [[ "$COUNT" -eq 0 ]]
  then
    echo "Got no features" 1>&2
    exit 1
  fi
  echo "Found '$COUNT' features" 1>&2
  touch "$out"
''