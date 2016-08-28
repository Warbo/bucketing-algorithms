defs: with defs; with lib; with quickspecBench;

mapAttrs (name: testRun name null { buildInputs = [ package ]; }) {

  failOnGarbage = ''
    if echo '!"£$%^&*()' | quickspecBench 1> stdout 2> stderr
    then
      cat stderr stdout 1>&2
      exit 1
    fi
    exit 0
  '';

  acceptSmtlib = ''
    quickspecBench < ${./example.smt2} > /dev/null || exit 1
  '';

  acceptSmtlibFile = ''
    ${names.SMT_FILE}="${./example.smt2}" quickspecBench
  '';

  genSig = ''
    OUT_DIR="${./testPackage}" ANNOTATED="${./annotated.json}" SIG="$PWD" \
      "${mkQuickSpecSig}"

    for F in ./sig.hs ./bench.sh ./runhaskell.sh
    do
      [[ -e "$F" ]] || {
        echo "Couldn't find '$F'" 1>&2
        exit 1
      }
    done
  '';

  runSig = ''
    OUT_DIR="${./testPackage}" ANNOTATED="${./annotated.json}" SIG="$PWD" \
      "${mkQuickSpecSig}"
    nix-shell -E 'import ./env.nix' ./bench.sh < sig.hs || {
      echo "START SIG"   1>&2
      cat         sig.hs 1>&2
      echo   "END SIG"   1>&2

      echo "START CMD" 1>&2
      cat         cmd  1>&2
      echo   "END CMD" 1>&2

      exit 1
    }
  '';

  getJsonOutput = ''
    BENCH_OUT=$(quickspecBench < "${./example.smt2}") || exit 1

    TYPE=$(echo "$BENCH_OUT" | jq -r 'type') || exit 1

    [[ "x$TYPE" = "xobject" ]] || {
      echo -e "START BENCH_OUT\n\n$BENCH_OUT\n\nEND BENCH_OUT" 1>&2
      echo "'$TYPE' is not object" 1>&2
      exit 1
    }
  '';
}