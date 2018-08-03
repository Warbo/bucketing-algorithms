{ fail, jq, makeDupeSamples, python3, runCommand, withDeps, wrap }:

with builtins;
with rec {
  go = wrap {
    name   = "dedupe.py";
    paths  = [ python3 ];
    vars   = { LANG = "en_US.UTF-8"; };
    script = ''
      #!/usr/bin/env python3
      import json
      import sys

      data = json.loads(sys.stdin.read())
      for size in data:
        seen = []
        for iRep in sorted([int(rep) for rep in data[size]]):
          rep             = str(iRep)
          sample          = frozenset(data[size][rep])
          data[size][rep] = None if sample in seen \
                                 else {'sample': data[size][rep]}
          seen += [sample]
      print(json.dumps(data))
    '';
  };

  tests = {
    canDedupe = runCommand "can-dedupe-samples"
      {
        inherit go makeDupeSamples;
        buildInputs = [ fail jq ];

        # 100 samples of size 1 will definitely contain duplicates
        reps  = "100";
        sizes = "[1]";
      }
      ''
        echo "Making samples containing duplicates" 1>&2
        "$makeDupeSamples" > samples.json

        jq -e '.["1"] | keys | length | . == 100' < samples.json ||
          fail "Expected 100 reps"

        jq -e '.["1"] | map(type | . == "array") | all' < samples.json || {
          cat samples.json 1>&2
          fail "Expected all reps to be arrays"
        }

        jq -e '.["1"] | map(length | . == 1) | all' < samples.json || {
          cat samples.json 1>&2
          fail "Expected all reps to have 1 entry"
        }

        echo "Deduplicating samples" 1>&2
        "$go" < samples.json > dedupe.json

        jq -e '.["1"] | keys | length | . == 100' < dedupe.json || {
          cat dedupe.json 1>&2
          fail "Should still have 100 reps"
        }

        jq -e '.["1"] | map(select(type | . == "array"))
                      | length | . < 100' < dedupe.json || {
          cat dedupe.json 1>&2
          fail "Some reps should have become null"
        }

        mkdir "$out"
      '';
  };
};
withDeps (attrValues tests) go
