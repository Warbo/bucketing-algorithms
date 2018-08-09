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
      from json       import dumps, loads
      from os         import getenv
      from subprocess import PIPE, Popen
      from sys        import stderr, stdin
      from time       import sleep

      msg  = lambda x: (stderr.write((x if type(x) == type("") \
                                        else repr(x)) + '\n'),
                        stderr.flush())

      sort = lambda collection: sorted([elem for elem in collection])

      key, prog = map(getenv, ['key', 'prog'])

      def setIn(path, want, val):
        assert path != [], "Can't set empty path"

        if len(path) == 1:
          val[path[0]] = want
          return val

        if path[0] not in val:
          val[path[0]] = {}

        returnsetIn(path[1:], want, val[path[0]])

      jobs = []

      def recurse(path, val):
        """Recurses through JSON structure, finding samples to bucket and noting
        the paths that we need to write the results to."""
        if val is None:
          return None
        if type(val) == type({}):
          if 'sample' in val:
            jobs.append({
              'path'  : path + [key],
              'input' : val['sample'],
            })
          return {k: recurse(path + [k], val[k]) for k in sort(val)}
        return val

      # Load the given set of samples
      data = loads(stdin.read())

      # Get data for all of the subprocesses we need to run
      recurse([], data)

      # Run 'jobs' via a pool of concurrent subprocesses
      total    = len(jobs)
      running  = []
      limit    = 20
      reported = 0  # The last value we reported to the user
      while jobs or running:
        # Output some progress information
        current = len(jobs) + len(running)
        if current != reported:
          msg(str(total - current) + '/' + str(total))
          reported = current

        # Try to fill up 'running', if there are any remaining jobs
        while (len(running) < limit) and jobs:
          details = jobs.pop(0)
          proc    = Popen([prog], stdin=PIPE, stdout=PIPE)
          proc.stdin.write(dumps(details['input']).encode('utf-8'))
          running.append({
            'proc' : proc,
            'path' : details['path'],
            'done' : False
          })

        # Look for finished processes, and update 'data' accordingly
        someDone = False
        for run in running:
          proc = run['proc']
          if proc.poll() is None:
            continue

          assert proc.returncode == 0, repr({
            'error': 'Subprocess gave non-zero return code',
            'code' : proc.returncode
          })

          someDone    = True
          run['done'] = True
          setIn(proc['path'], loads(proc.stdout.read().decode('utf-8')), data)

        # Now drop those finished jobs that we processed (we don't poll again,
        # since that would be a race condition)
        if someDone:
          running = list(filter(lambda run: not run['done'], running))

        # Wait a little, since this is a busy-loop
        sleep(0.1)

      print(dumps(data))
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
