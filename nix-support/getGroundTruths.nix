{ fail, jq, makeSamples, runCommand, tebenchmark, withDeps, wrap }:

with rec {
  go = wrap {
    name   = "get-ground-truths.rkt";
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

  test = runCommand "test-get-ground-truths"
    {
      inherit go;
      buildInputs = [ fail jq ];
      samples     = makeSamples { sizes = [ 1 5 10 ]; reps = 3; };
    }
    ''
      O=$("$go" < "$samples") || fail "Failed to add ground truths"
      echo "$O" | jq -e 'type | . == "object"' || fail "Didn't get object\n$O"
      echo "$O" > "$out"
    '';
};
withDeps [ test ] go
