/*
Reads in JSON bucketing results, of the form:

  {
    size1 : {
      rep1 : {
        "sample" : {
          "names" : [
            name1,
            ...
          ],
          "theorems" : [
            theorem1,
            ...
          ]
        },
        method1 : {
          bucketSize1 : {
            "names" : [
              [
                name1,
                ...
              ],
              ...
            ],
            "theorems" : [
              theorem1,
              ...
            ]
          },
          ...
        },
        ...
      },
      ...
    },
    ...
  }

For each rep of each size, the "sample" entry contains all of the sampled names
and the full ground truth. Each sibling of "sample" contains the results of a
bucketing method. Each bucketing method has entries for a variety of different
(target, or average) bucket sizes; for example "1" will (attempt to) put each
name in its own bucket, "5" will (attempt to) put five names in each bucket,
etc. Note that we say 'target', 'average' and 'try to', since each method may
prefer to keep some names together even if other buckets are available.

For each size the "names" entry shows how the names were divided into buckets
(conceptually this is a set of sets of names; the order is irrelevant). The
"theorems" entry shows the union of the ground truths of the buckets.

The purpose of this script is to calculate how much these 'unions of ground
truths of buckets' differ from the 'ground truth of the union of buckets' (i.e.
the full ground truth, if no bucketing were used).
*/
{ fail, jq, python3, runCommand, withDeps, wrap }:

with rec {
  cmd = wrap {
    name   = "calculateProportions";
    paths  = [ python3 ];
    script = ''
      #!/usr/bin/env python3
      from functools import reduce
      import json
      import sys

      ## Helpers

      concat = lambda xs: reduce(lambda x, y: x + y, xs, [])

      ## Data processors, one for each 'level'

      def process(data):
        return {s: processSize(x) for s, x in data.items()}

      def processSize(data):
        return {r: processRep(x) for r, x in data.items()}

      def processRep(data):
        return {m: x if m == "sample" else processMethod(x, data["sample"]) \
                for m, x in data.items()}

      def processMethod(data, sample):
        return {bs: processBuckets(x, sample) for bs, x in data.items()}

      def processBuckets(data, sample):
        bucketNames = concat(data['names'])
        assert sorted(bucketNames) == sorted(sample['names']), repr({
          'error'       : "Buckets don't contain all names",
          'bucketNames' : bucketNames,
          'sampleNames' : sample['names']
        })

        bucketTheorems = data['theorems']
        assert all((t in sample['theorems'] for t in bucketTheorems)), repr({
          'error'          : 'Buckets have theorems from outside ground truth',
          'bucketTheorems' : bucketTheorems,
          'sampleTheorems' : sample['theorems']
        })

        found = len(bucketTheorems)
        avail = len(sample['theorems'])
        return dict(data, comparison={
          'found'      : found,
          'available'  : avail,
          'missing'    : avail - found,
          'proportion' : float(found) / float(avail)
        })

      print(json.dumps(process(json.loads(sys.stdin.read()))))
    '';
  };

  test = runCommand "test-calculateProportions"
    {
      inherit cmd;
      buildInputs = [ fail jq ];
    }
    ''
      set -o pipefail
      mkdir "$out"

      O=$(echo '{}' | "$cmd" | tee "$out/empty.json") ||
        fail "Failed on empty object\n$O"
      echo "$O" | jq -e '. == {}' || fail "Wanted {}, got:\n$O"

      O=$(echo '{"1":{"1":{"foo":{"1":{"names":["x"],"theorems":["y"]}}}}}' |
          "$cmd" 2>&1 | tee "$out/nosample.json") &&
        fail "Should've failed without 'sample'\n$O"

      O=$(echo '{"1":{"1":{"sample":{"names"   :["n"],
                                     "theorems":["t"]},
                           "m":{"1":{"names"   :[["n"],["m"]],
                                     "theorems":[]}}}}}' |
          "$cmd" 2>&1 | tee "$out/nameerror.json") &&
        fail "Should've failed with differing names\n$O"

      O=$(echo '{"1":{"1":{"sample":{"names"   :["n"],
                                     "theorems":["t","u","v"]},
                           "m":{"1":{"names"   :[["n"]],
                                     "theorems":["u"]}}}}}' |
          "$cmd" | tee "$out/onethird.json") ||
        fail "Failed when given valid input\n$O"

      C=$(echo "$O" | jq '.["1"] | .["1"] | .m | .["1"] | .comparison') ||
        fail "Couldn't extract comparison data from:\n$O"
      echo "$C" | jq -e '. == {"found"     :1,
                               "available" :3,
                               "missing"   :2,
                               "proportion":0.3333333333333333}' ||
        fail "Comparison data didn't match expected:\n$C\nTaken from:\n$O"

      true
    '';
};
withDeps [ test ] cmd
