# Choose a bunch of samples, bucket them in a variety of ways and measure the
# proportion of ground truth theorems which apply to the resulting buckets.
#
# Write output to JSON for archiving.
{ benchmarkingCommands, jq, lib, makeSamples, nixpkgs, runCommand, tebenchmark,
  testData, wrap }:
with { inherit (builtins) concatStringsSep map; };

given: with rec {
  samplingParams = if given != {}
                      then given
                      else { maxSize = 100; reps = 100; };

  samples = makeSamples samplingParams;

  # Runs each sample through the stdio of a given program, adding the result to
  # the samples JSON. Useful for running a bucketing script on each sample.
  processSamples = { key, prog, samples }: runCommand "processed-${key}.json"
    {
      inherit samples;
      script = wrap {
        name  = "process-samples.py";
        paths = [ nixpkgs.python3 ];
        vars  = { inherit key prog; };
        script = ''
          #!/usr/bin/env python3
          from io         import StringIO
          from json       import dumps, loads
          from os         import getenv
          from subprocess import check_output
          from sys        import stderr, stdin

          msg  = lambda x: stderr.write(repr(x) + '\n')

          sort = lambda collection: sorted([elem for elem in collection])

          key, prog = map(getenv, ['key', 'prog'])

          process = lambda names: loads(check_output(
            [prog],
            input=dumps(names).encode('utf-8')).decode('utf-8'))

          def recurse(path, val):
            if val is None:
              return None
            if type(val) == type({}):
              if 'sample' in val:
                return dict(val, **{key: recurse(path + ['sample'],
                                                 val['sample'])})
              return {k: recurse(path + [k], val[k]) for k in sort(val)}
            if path != [] and path[-1] == 'sample':
              return process(val)
            return val

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
      inherit (benchmarkingCommands) getGroundTruths;
    }
    ''"$getGroundTruths" < "$samples" > "$out"'';

  withBuckets = addHashBuckets samples;

  hashTruths = groundTruthsOf withBuckets;

  fullTruths = groundTruthsOf samples;

  hashProportions = throw "hashProportions not implemented";
};
{ inherit fullTruths hashTruths withBuckets; }
