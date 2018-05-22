# Commands useful for generating and bucketing test data. These have TEBenchmark
# baked in, so we only need to provide a sample of names and their corresponding
# ASTs will be looked up internally.
#
# We benchmark these in two ways:
#
#  - The impact of the bucketing algorithms is measured, on lots of test data
#  - The speed of the scripts is measured on small inputs, to aid us in
#    optimising their implementation (since the above can be very slow!)
{ bash, buckets, fail, jq, lib, nixpkgs, runCommand, tebenchmark, testData,
  withDeps, wrap }:

with builtins;
rec {
  astsOf = wrap {
    name   = "astsOf";
    paths  = [ bash jq ];
    vars   = { inherit (testData.tip-benchmark) asts; };
    script = ''
      #!/usr/bin/env bash
      set -e

      # Takes a JSON array of names, returns a JSON array of their ASTs
      jq --slurpfile asts "$asts" \
        'sort | map(. as $name | $asts[0] | map(select(.name == $name))) | add'
    '';
  };

  # Run the bucket script on each sample; we use a few bucket sizes, in
  # increments
  addHashBucketsCmd = wrap {
    name  = "hash";
    paths = [ buckets.hashes jq ];
    vars  = {
      inherit astsOf;
      sizes = concatStringsSep " " (map toString (lib.range 1 20));
    };
    script = ''
      #!/usr/bin/env bash
      set -e
      ASTS=$("$astsOf")

      for CLUSTER_SIZE in $sizes
      do
        export CLUSTER_SIZE
        echo "$ASTS" | hashBucket |
          jq '{(env["CLUSTER_SIZE"]) : map(map(.name))}'
      done | jq -s 'add'
    '';
  };

  makeDupeSamples =
    with rec {
      script = wrap {
        name   = "makeDupeSamples.rkt";
        paths  = [ tebenchmark.env ];
        vars   = tebenchmark.cache;
        script = ''
          #!/usr/bin/env racket
          #lang racket
          (require json)
          (require lib/sampling)

          (define sizes
            (if (getenv "sizes")
                (string->jsexpr (getenv "sizes"))
                (if (getenv "maxSize")
                    (range 1 (+ 1 (string->number (getenv "maxSize"))))
                    (error "No sizes or maxSize env var given"))))

          (define reps
            (range 0 (string->number (getenv "reps"))))

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
                                    reps))))
                   sizes)))
        '';
      };

      go = env: runCommand "test-makeDupeSamples-${env.tag}"
                           (env // {
                             inherit script;
                             reps        = "2";
                             buildInputs = [ fail jq ];
                           });

      testBroke = go { tag = "broke"; } ''
        if "$script"
        then
          fail "Shouldn't have succeeded without maxSize or sizes"
        fi
        mkdir "$out"
      '';

      testMax = go { tag = "max"; maxSize = "3"; } ''
        "$script" > result

        function go {
          if jq -e "$1" < result
          then
            echo "PASS: $2" 1>&2
          else
            cat result 1>&2
            fail "FAIL: $2"
          fi
        }

        go 'type | . == "object"' "Sizes are object"
        go 'keys | sort | . == ["1", "2", "3"]' "Sizes are 1,2,3"
        go 'map_values(type | . == "object") | all' "Reps are objects"
        go 'map_values(keys | sort | . == ["0", "1"]) | all' "Reps are 0,1"
        for SIZE in 1 2 3
        do
          go ".[\"$SIZE\"] | map_values(length | . == $SIZE) | all" \
             "Sizes are $SIZE"
        done
        mkdir "$out"
      '';

      testSizes = go { tag = "sizes"; sizes = toJSON [ 1 5 14 2 ]; } ''
        "$script" > result

        function go {
          if jq -e "$1" < result
          then
            echo "PASS: $2" 1>&2
          else
            cat result 1>&2
            fail "FAIL: $2"
          fi
        }

        go 'type | . == "object"' "Sizes are object"
        go 'keys | sort | . == ["1", "14", "2", "5"]' "Sizes are 1,2,5,14"
        go 'map_values(type | . == "object") | all' "Reps are objects"
        go 'map_values(keys | sort | . == ["0", "1"]) | all' "Reps are 0,1"
        for SIZE in 1 2 5 14
        do
          go ".[\"$SIZE\"] | map_values(length | . == $SIZE) | all" \
             "Sizes are $SIZE"
        done
        mkdir "$out"
      '';
    };
    withDeps [ testBroke testMax testSizes ] script;

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
