defs: with defs; pkg: with pkg;

drvFromScript { buildInputs = [ jq ]; } ''
  set -e
  jq -c -r '.[] | .module + "." + .name' < "${annotated}" |
  while read -r LINE
  do
    "${jq}/bin/jq" -r '.cmd' < "${pkg.ranTypes}" |
      grep "('$LINE)" > /dev/null || {
        echo "$LINE not in '${name}' type command" 1>&2
        exit 1
      }
  done
  touch "$out"
''
