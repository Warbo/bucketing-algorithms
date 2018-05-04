# Commands useful for generating and bucketing test data. These have TEBenchmark
# baked in, so we only need to provide a sample of names and their corresponding
# ASTs will be looked up internally.
#
# We benchmark these in two ways:
#
#  - The impact of the bucketing algorithms is measured, on lots of test data
#  - The speed of the scripts is measured on small inputs, to aid us in
#    optimising their implementation (since the above can be very slow!)
{ buckets, jq, lib, nixpkgs, tebenchmark, testData, wrap }:

with builtins;
{
  # Run the bucket script on each sample; we use a few bucket sizes, in
  # increments
  addHashBucketsCmd = wrap {
    name  = "hash";
    paths = [ buckets.hashes jq ];
    vars  = {
      inherit (testData.tip-benchmark) asts;
      sizes = concatStringsSep " " (map toString (lib.range 1 20));
    };
    script = ''
      #!/usr/bin/env bash
      set -e
      INPUT=$(sort)
       ASTS=$(jq -s --slurpfile asts "$asts" 'map($asts[0][.])')

     echo -e "INPUT:\n$INPUT\n\nASTS:\n$ASTS\n\n" 1>&2

      for CLUSTER_SIZE in $sizes
      do
        export CLUSTER_SIZE
        echo "$ASTS" | hashBucket |
          jq '{(env["CLUSTER_SIZE"]) : map(map(.name))}'
      done | jq -s 'add'
    '';
  };

  makeDupeSamples = wrap {
    name   = "script.rkt";
    paths  = [ tebenchmark.env ];
    vars   = tebenchmark.cache;
    script = ''
      #!/usr/bin/env racket
      #lang racket
      (require json)
      (require lib/sampling)

      (write-json
        (make-immutable-hash
          (map (lambda (size)
                 (cons (string->symbol (~a size))
                       (make-immutable-hash
                         (map (lambda (rep)
                                (eprintf
                                  (format "Size ~a rep ~a\n" size rep))
                                (cons (string->symbol (~a rep))
                                      (map ~a
                                        (set->list
                                          (sample-from-benchmarks size
                                                                  rep)))))
                                (range 0 (string->number (getenv "reps")))))))
               (range 1 (+ 1 (string->number (getenv "maxSize")))))))
    '';
  };

  dedupeSamples = wrap {
    name = "dedupe.py";
    paths = [ nixpkgs.python3 ];
    script = ''
      #!/usr/bin/env python3
      import json
      import sys

      data = json.loads(sys.stdin.read())
      for size in data:
        seen = []
        for iRep in sorted([int(rep) for rep in data[size]]):
          rep = str(iRep)
          sample = frozenset(data[size][rep])
          if sample in seen:
            data[size][rep] = None
          else:
            data[size][rep] = {'sample': data[size][rep]}
          seen += [sample]
      print(json.dumps(data))
    '';
  };
}
