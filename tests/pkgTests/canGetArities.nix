defs: with defs; pkg:

let typeResults = runScript (withNix {}) ''
      set -e
      "${jq}/bin/jq" -r '.result' < "${testRunTypes."${pkg.name}"}" \
                                  > typeResults.json
      "${storeResult}" typeResults.json "$out"
    '';

    arities = runScript (withNix { buildInputs = [ adb-scripts ]; }) ''
      set -e
      getArities < "${typeResults}" > arities.json
      "${storeResult}" arities.json "$out"
    '';

    count = fromJSON (parseJSON (runScript {} ''
              "${jq}/bin/jq" -r 'length' < "${arities}" > "$out"
            ''));
 in assertMsg (count > 0) "Found '${count}' arities for '${pkg.name}'"
