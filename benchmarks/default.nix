# Builds the environment in which to run a benchmark. This will be called from
# asv, passing in dependencies as arguments.
{
  dir  ? ./.., # Path to the revision containing the benchmarks
  root ? ./.., # Path to the revision being benchmarked
  ...
}:

with builtins;
with trace (toJSON { inherit dir root; }) {
  fixed    = import "${dir }";
  measured = import "${root}";
};
with fixed.nixpkgs1609.lib;

fixed.mkBin {
  name  = "python";
  paths = with fixed.nixpkgs1609; [ (python3.withPackages (p: [])) ];
  vars  = {
    inherit root;
    LANG = "en_US.UTF-8";

    # All of the scripts to benchmark should be in here, taken from measured
    commands = toJSON {
      inherit (measured.benchmarkingCommands)
        addHashBucketsCmd addRecurrentBucketsCmd astsOf dedupeSamples
        getGroundTruths;

      # Rather than taking makeDupeSamples directly from benchmarkingCommands,
      # we wrap it with a Python script to avoid spewing info messages to stderr
      # and to set the env vars that are needed to control the sampling.
      sample = with fixed; wrap {
        name   = "sample";
        paths  = [ nixpkgs1803.python3 ];  # For subprocess.run
        vars   = { reps  = "2"; sizes = toJSON (range 1 100); };
        script = ''
          #!/usr/bin/env python3
          import re
          import subprocess
          import sys

          fixed = [
            'Converted all theorem dependencies into constraints',
            'Calculated frequency for each constraint',
            'Shuffling names',
            'Obtained sample'
          ]

          regexen = map(re.compile, [
            '^Sampling \d* names from a total of \d*$'
            '^Size \d* rep \d*$'
          ])

          def unexpected(line):
            """Whether a line of stderr is expected. This lets us filter out
            noise which ASV would otherwise print out."""
            s = line.strip()
            if s in fixed: return False
            for r in regexen:
              if r.match(s): return False
            return True

          def writeErr(p):
            """Output the given process's stderr, after filtering out expected
            lines."""
            sys.stderr.write('\n'.join(filter(unexpected,
                                              p.stderr.split('\n'))))

          try:
            p = subprocess.run(
              ["${measured.benchmarkingCommands.makeDupeSamples}"],
              capture_output = True,
              text           = True,
              check          = True)
          except:
            writeErr(p)
            raise
          writeErr(p)
          print(p.stdout)
        '';
      };
    };

    # A fixed set of samples, for scripts which need them as input
    samples = fixed.makeSamples {
      sizes = [ 1 5 10 15 20 ];
      reps  = 5;
    };
  };
  script = ''
    #!/usr/bin/env bash
    exec python3 "$@"
  '';
}
