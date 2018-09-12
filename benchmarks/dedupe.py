from os     import getenv
from timeit import default_timer
from .util  import load_command, load_samples, run_on

cmd = load_command('dedupeSamples')

with open(getenv('samples'), 'r') as f:
  samples = f.read()

def time_dedupe():
    run_on([cmd], samples)

time_dedupe.timer   = default_timer  # Wall-clock rather than CPU time
time_dedupe.timeout = 600            # Can be slow
