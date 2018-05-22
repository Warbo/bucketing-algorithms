{ benchmarkingCommands, composeWithArgs, runCommand }:

with builtins;
with {
  # Gather samples: run 'choose_sample size rep' for each size up to maxSize and
  # rep from 0 to reps-1. Returns an object mapping sizes to objects, where the
  # inner objects map reps to lists of name strings. We use the name 'dupe'
  # because we will end up with duplicate samples for small sizes. We do the
  # looping in Racket so we can call the sampler without the overhead of
  # invoking a fresh Racket process each time.
  dupeSamples = { maxSize ? null, reps, sizes ? null }:
    assert !(maxSize == null && sizes == null) || abort "No maxSize XOR sizes";
    assert maxSize == null -> isList sizes     || abort "sizes should be list";
    assert maxSize == null -> all isInt sizes  || abort "sizes should be ints";
    assert sizes   == null -> isInt  maxSize   || abort "maxSize should be int";
    runCommand "samples-with-dupes.json"
      {
        inherit (benchmarkingCommands) makeDupeSamples;
        maxSize = if maxSize == null then null else toJSON maxSize;
        sizes   = if sizes   == null then null else toJSON sizes;
        reps    = toString reps;
      }
      ''
        "$makeDupeSamples" > "$out"
      '';

  # Deduplicate the raw samples: duplicates are replaced with null, whilst
  # non-duplicates are set as the "sample" key of an object.
  deduper = dupes: runCommand "samples.json"
    {
      inherit dupes;
      inherit (benchmarkingCommands) dedupeSamples;
    }
    ''
      "$dedupeSamples" < "$dupes" > "$out"
    '';
};

{ maxSize ? null, reps, sizes ? null }@args: deduper (dupeSamples args)
