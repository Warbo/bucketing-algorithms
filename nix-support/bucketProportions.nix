# Scripts for bucketing samples in a variety of ways and measure the
# proportion of ground truth theorems which apply to the resulting buckets.
#
# Write output to JSON for archiving.
{ averageProportions, benchmarkingCommands, calculateProportions, composeBins,
  jq, lib, python3, runCommand, runOn, tebenchmark, wrap }:

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

      msg  = lambda x: (stderr.write((x if type(x) == type("") \
                                        else repr(x)) + '\n'),
                        stderr.flush())

      sort = lambda collection: sorted([elem for elem in collection])

      key, prog = map(getenv, ['key', 'prog'])

      process = lambda names: loads(check_output(
        [prog],
        input=dumps(names).encode('utf-8')).decode('utf-8'))

      done = []

      def recurse(path, val):
        if len(path) == 1:
          done.append(path[0])
          msg(str(len(done)) + '/' + str(len(data)))
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

  addRecurrent = processSamplesScript {
    key  = "recurrent";
    prog = benchmarkingCommands.addRecurrentBucketsCmd;
  };

  addHashed = processSamplesScript {
    key  = "hashed";
    prog = benchmarkingCommands.addHashBucketsCmd;
  };
};

rec {
  inherit addRecurrent addHashed averageProportions calculateProportions
          combinedScript;
  inherit (benchmarkingCommands) getGroundTruths;
}
