# Choose a bunch of samples, bucket them in a variety of ways and measure the
# proportion of ground truth theorems which apply to the resulting buckets.
#
# Write output to JSON for archiving.
{ averageProportions, benchmarkingCommands, calculateProportions, composeBins,
  jq, lib, makeSamples, python3, runCommand, runOn, tebenchmark, wrap }:
with { inherit (builtins) concatStringsSep map; };

with rec {
  # Runs each sample through the stdio of a given program, adding the result to
  # the samples JSON. Useful for running a bucketing script on each sample.
  processSamplesScript = { key, prog }: wrap {
    name   = "process-samples.py";
    paths  = [ (python3.withPackages (p: [])) ];
    vars   = { inherit key prog; LANG = "en_US.UTF-8"; };
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

  proportionsScript = composeBins "proportions-script" [
    (processSamplesScript {
      key  = "recurrent";
      prog = benchmarkingCommands.addRecurrentBucketsCmd;
    })
      (processSamplesScript {
      key  = "hashed";
      prog = benchmarkingCommands.addHashBucketsCmd;
    })
    benchmarkingCommands.getGroundTruths
    calculateProportions
  ];
};

{ maxSize, reps }: rec {
  inherit proportionsScript;
  averageProportionsScript = averageProportions;
  samples                  = makeSamples { inherit maxSize reps; };
  usageNotes               = ''
    The 'samples' derivation will build a JSON file of samples, according to the
    given 'maxSize' and 'reps' arguments.

    The 'proportionsScript' derivation builts a script. This script can be sent
    the samples JSON on stdin, and it will run all bucketing algorithms on each
    sample to produce JSON on stdout containing the given samples, annotated
    with the buckets and ground truths (full and bucketed) for each method.

    The 'averageProportionsScript' derivation builds a script. This script can
    be sent the output of 'proportionsScript' on stdin, and will produce on
    stdout some JSON which averages the ground truth proportions.
  '';
}
