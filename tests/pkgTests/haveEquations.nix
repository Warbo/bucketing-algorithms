defs: with defs; pkg:
with builtins;

let check = label: c: data: parseJSON (runScript { buildInputs = [ jq ]; } ''
      set -e
      echo "Looking for equations in '${data}' (${label})" 1>&2
      jq -s 'length | . > 0' < "${data}" > "$out"
    '');
    result   = src: label: c:
      assertMsg (mapAttrs (check label) src)."${toString c}"
                "Equations for '${pkg.name}.${label}' in ''${toString c}' clusters";
    checkAll = label: src: assertMsg (all (result src label) defaultClusters)
                                    "All clusters of ${label} have equations";
 in checkAll "explored"    pkg.explored &&
    checkAll "preExplored" pkg.preExplored
