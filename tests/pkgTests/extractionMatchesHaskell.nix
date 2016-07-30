defs: with defs; pkg:

# extractFeatures is written in bash + jq, and is really slow. We've
# replaced it with ml4hsfe-loop, but keep it around for testing
drvFromScript { buildInputs = [ jq ML4HSFE ]; } ''
  set -e
  BASH_RESULT=$("${recurrent-clustering}/lib/extractFeatures" < "${pkg.annotated}" | jq '.') || {
    echo "Couldn't extract features with bash: $BASH_RESULT" 1>&2
    exit 2
  }

  RESULT=$(jq -n --argfile bash    <(echo "$BASH_RESULT")    \
              --argfile haskell "${pkg.features}" \
              '$bash == $haskell')

  if [[ "x$RESULT" = "xtrue" ]]
  then
    touch "$out"
  else
    echo "RESULT is '$RESULT'" 1>&2
    exit 1
  fi
''
