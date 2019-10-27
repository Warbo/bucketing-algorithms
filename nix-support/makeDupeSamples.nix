{ fail, jq, runCommand, tebenchmark, withDeps, wrap }:

with builtins;
with rec {
  script = wrap {
    name   = "makeDupeSamples.rkt";
    paths  = [ tebenchmark.env ];
    vars   = tebenchmark.cache;
    script = ''
      #!${tebenchmark.env}/bin/racket
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
withDeps [ testBroke testMax testSizes ] script
