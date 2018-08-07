{ runCommand }:

name: cmd: f: runCommand name { inherit cmd f; } ''
  "$cmd" < "$f" > "$out"
''
