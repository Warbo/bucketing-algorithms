{ benchmark, dump-package, parseJSON, perl, runScript, stdenv, writeScript }:
{ quick, pkgDir }:

runScript {} ''
  set -e

  D="${toString pkgDir}"

  [[ -d "$D" ]] || {
    echo "Couldn't find directory to dump '$D'" 1>&2
    exit 1
  }

  cp -r "$D" ./pkgDir
  chmod +w -R pkgDir

  echo "Dumping '$D'" 1>&2
  HOME="$PWD" DIR="$PWD/pkgDir" \
    "${benchmark quick dump-package []}" > "$out"
''
