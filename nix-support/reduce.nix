{ benchmark, checkFailures, drvFromScript, explore, haskellPackages, lib,
  reduce-equations, stdParts, storeParts, writeScript }:
with builtins;
with lib;

rec {

script = writeScript "reduce-equations" ''
  INPUT=$(cat)
  echo "Got input: $INPUT" 1>&2
  echo "$INPUT" | reduce-equations 1> "$TEMPDIR/reduce-stdout" \
                                   2> "$TEMPDIR/reduce-stderr"

  SOUT=$(cat "$TEMPDIR/reduce-stdout")
  SERR=$(cat "$TEMPDIR/reduce-stderr")
  echo -e "STDOUT START\n\n$SOUT\n\nSTDOUT END" 1>&2
  echo -e "STDERR START\n\n$SERR\n\nSTDERR END" 1>&2

  cat "$TEMPDIR/reduce-stdout"
'';

doReduce = quick: clusterCount: inputs:
             drvFromScript { inherit inputs;
                             outputs = stdParts;
                             buildInputs = explore.extractedEnv {
                                             extraHs = [ "reduce-equations" ];
                                           }; } ''
               set -e
               export CLUSTERS="${clusterCount}"

               function getEqs {
                 for F in $inputs
                 do
                   jq -c '.[]' < "$F"
                 done
               }

               O=$(getEqs | "${benchmark {
                               inherit quick inputs;
                               cmd = toString script;
                             }}")
               ${storeParts}
             '';

reduce = { quick, explored }:
  let results = mapAttrs (doReduce quick) explored;
      failed  = checkFailures "any" results;
      result  = { inherit results failed; };
   in result;
}
