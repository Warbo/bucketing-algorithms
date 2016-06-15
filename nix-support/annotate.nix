{ adb-scripts, annotateAstsScript, benchmark, getDepsScript, jq, parseJSON,
  runScript, runTypesScript, writeScript }:
{ asts, pkg, pkgSrc ? null, quick }:

let annotateDb = writeScript "annotateDb" ''
      #!/usr/bin/env bash

      # Turns output from dump-package or dump-hackage into a form suitable for ML4HS.

      "${runTypesScript { inherit pkg pkgSrc; }}" |
        "${annotateAstsScript}"                   |
        "${getDepsScript}"
    '';

 in parseJSON (runScript { buildInputs = [ adb-scripts ]; } ''
      set -e
      "${benchmark quick annotateDb []}" < "${asts}" > "$out"
    '')
