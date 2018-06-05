# Choose a bunch of samples, bucket them in a variety of ways and measure the
# proportion of ground truth theorems which apply to the resulting buckets.
#
# Write output to JSON for archiving.
{ benchmarkingCommands, buckets, jq, lib, makeSamples, nixpkgs, runCommand,
  tebenchmark, testData, wrap }:
with { inherit (builtins) concatStringsSep map; };

{ samplingParams ? { maxSize = 100; reps = 100; } }: rec {

  samples = makeSamples samplingParams;

  # Runs each sample through the stdio of a given program, adding the result to
  # the samples JSON. Useful for running a bucketing script on each sample.
  processSamples = { key, prog, samples }: runCommand "processed-${key}.json"
    {
      inherit samples;
      script = wrap {
        name  = "process-samples.py";
        paths = [ nixpkgs.python3 ];
        vars  = {
          inherit key prog;
          inherit (testData.tip-benchmark) asts;
        };
        script = ''
          #!/usr/bin/env python3
          from io         import StringIO
          from json       import dumps, loads
          from os         import getenv
          from subprocess import check_output
          from sys        import stderr, stdin

          msg  = lambda x: stderr.write(repr(x) + '\n')

          sort = lambda collection: sorted([elem for elem in collection])

          asts = {x['name']: x for x in loads(open(getenv('asts'), 'r').read())}

          astsFor = lambda names: [asts[name] for name in sort(names)]

          prog = getenv('prog')

          process = lambda names: loads(check_output(
            [prog],
            input=dumps(astsFor(names)).encode('utf-8')).decode('utf-8'))

          def recurse(path, val):
            if val is None:
              return None
            if type(val) == type({}):
              if 'sample' in val:
                return dict(val, sample=recurse(path, val['sample']))
              return {k: recurse(path + [k], val[k]) for k in sort(val)}
            msg(path)
            return process(val)

          data = loads(stdin.read())

          print(dumps(recurse([], data)))
        '';
      };
    }
    ''
      "$script" < "$samples" > "$out"
    '';

  addHashBuckets = samples: processSamples {
    inherit samples;
    key  = "hashed";
    prog = benchmarkingCommands.addHashBucketsCmd;
  };

  groundTruthsOf = samples: runCommand "ground-truths.json"
    {
      inherit samples;
      script = wrap {
        name   = "ground-truths-of.rkt";
        paths  = [ tebenchmark.env ];
        vars   = tebenchmark.cache;
        script = ''
          #!/usr/bin/env racket
          #lang racket
          (require        json)
          (require        rackunit)
          (require        lib/conjectures)
          (require/expose lib/conjectures (theorem-files-admitted-by))
          (require        lib/normalise)
          (require        lib/util)

          (define (err x)
            (error (format "~S" x)))

          ;; Ground truth for a sample (list of encoded names)
          (define (theorems-of-sample sample)
            (eprintf "theorems-of-sample\n")
            (unless (list? sample)
              (err `((error  "Expected sample to be a list")
                     (sample ,sample))))
            (theorem-files-admitted-by (map (compose decode-name string->symbol)
                                            sample)))

          ;; Ground truth for a list of samples (e.g. buckets)
          (define (theorems-of-sample-list samples)
            (eprintf "theorems-of-sample-list\n")
            (remove-duplicates
              (foldl (lambda (sample theorems)
                       (eprintf "\n~S\n" `((theorems ,theorems) (sample ,sample)))
                       (append theorems (theorems-of-sample sample)))
                     '()
                    samples)))

          ;; Takes a sample or list of samples, returns it with ground truth
          (define (add-ground-truth sample-or-list)
            (eprintf "add-ground-truth\n")
            (make-immutable-hash
              `((names    . ,sample-or-list)
                (theorems . ,(cond
                               [(not (list? sample-or-list))
                                (err `((error "Should have got a list")
                                       (sample-or-list ,sample-or-list)))]

                               [(empty? sample-or-list) '()]

                               [(string? (first sample-or-list))
                                (theorems-of-sample      sample-or-list)]

                               [(list?   (first sample-or-list))
                                (theorems-of-sample-list sample-or-list)]

                               [#t (err
                                 `((error "Unexpected list type")
                                   (sample-or-list ,sample-or-list)))])))))

          ;; Adds ground truths to any lists in the given map, recursively
          (define (add-ground-truths data)
            (eprintf "add-ground-truths\n")
            (hash-foldl (lambda (key value result)
                          (hash-set result key
                            (cond
                              [(list?  value) (add-ground-truth  value)]
                              [(hash?  value) (add-ground-truths value)]
                              [(equal? value 'null) value]

                              [#t (err `((error  "Unexpected type")
                                         (key    ,key)
                                         (value  ,value)
                                         (result ,result)))])))
                        (make-immutable-hash '())
                        data))

          ;; Add ground truths to stdio
          (write-json (add-ground-truths (string->jsexpr (port->string))))
        '';
      };
    }
    ''
      "$script" < "$samples" > "$out"
    '';

  withBuckets = addHashBuckets samples;

  hashTruths = groundTruthsOf withBuckets;
}
