{ calculateProportions, fail, jq, python3, runCommand, withDeps, wrap }:

with rec {
  cmd = wrap {
    name   = "averageProportions";
    paths  = [ python3 ];
    script = ''
      #!/usr/bin/env python3
      from functools import reduce
      import json
      import math
      import sys

      concat = lambda xs: reduce(lambda x, y: x + y, xs, [])

      def process(data):
        return {s: processSize(x) for s, x in data.items()}

      def processSize(data):
        methods = list(set(concat([list(data[rep].keys()) for rep in data])))
        return {m: processMethod(data, m) for m in methods if m != "sample"}

      def processMethod(data, method):
        bucketSizes = [list(data[rep][method].keys()) for rep in data]
        bucketSizes = list(set(concat(bucketSizes)))
        return {bs: processBucketSize(data, method, bs) for bs in bucketSizes}

      def processBucketSize(data, method, bSize):
        proportions = [data[rep][method][bSize]['comparison']['proportion'] \
                       for rep in data]

        count    = float(len(proportions))
        mean     = float(sum(proportions)) / count
        variance = sum([float(p - mean)**2 for p in proportions]) / (count - 1)
        stddev   = math.sqrt(variance)
        return {'proportion': {'mean'  : mean,
                               'stddev': stddev}}

      print(json.dumps(process(json.loads(sys.stdin.read()))))
    '';
  };

  test = runCommand "test-averageProportions"
    {
      inherit cmd;
      buildInputs = [ fail jq ];
    }
    ''
      O=$(echo '{}' | "$cmd") || fail "Failed on empty input\n$O"

      INPUT='{"1":{"1":{"sample":{"names":   ["n"],
                                  "theorems":["t1","t2"]},
                        "a":{"1":{"names":   [["n"]],
                                  "theorems":["t1"]},
                             "2":{"names":   [["n"]],
                                  "theorems":["t1","t2"]}},
                        "b":{"1":{"names":   [["n"]],
                                  "theorems":[]},
                             "2":{"names":   [["n"]],
                                  "theorems":["t2"]}}},
                   "2":{"sample":{"names":   ["n2"],
                                  "theorems":["t3"]},
                        "a":{"1":{"names":   [["n2"]],
                                  "theorems":["t3"]},
                             "2":{"names":   [["n2"]],
                                  "theorems":["t3"]}},
                        "b":{"1":{"names":   [["n2"]],
                                  "theorems":[]},
                             "2":{"names":   [["n2"]],
                                  "theorems":["t3"]}}}}}'

      PS=$(echo "$INPUT" | "${calculateProportions}") ||
        fail "Couldn't calculate proportions\n$PS"

      O=$(echo "$PS" | "$cmd") || fail "Couldn't average. In:\n$PS\nOut:\n$O"

      echo "$O" | jq -e '.["1"] | keys | sort | . == ["a", "b"]' ||
        fail "Didn't have methods 'a' and 'b'\n$O"

      echo "$O" | jq -e 'map(map(keys | sort | . == ["1", "2"]) | all) | all' ||
        fail "Methods didn't have bucket sizes 1 and 2\n$O"

      echo "$O" | jq -e 'map(map(map(.proportion | keys | sort |
                                     . == ["mean", "stddev"]) |
                                 all) |
                             all) |
                         all' ||
        fail "Bucket sizes didn't have mean and stddev\n$O"

      WANT='{"a": {"1": 0.75, "2": 1   },
             "b": {"1": 0,    "2": 0.75}}'
      GOT=$(echo "$O" | jq -e '.[] | map_values(map_values(.mean))')
      echo "$GOT" | jq --argjson want "$WANT" '. == $want' ||
        fail "Expected means differ. Want:\n$WANT\nGot:\n$GOT\nOriginal:\n$O"

      mkdir "$out"
    '';
};
withDeps [ test ] cmd
