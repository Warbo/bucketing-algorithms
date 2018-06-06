from timeit     import default_timer
from .util      import load_command, load_samples, run_on

cmd     = load_command('dedupeSamples')
samples = load_samples()

def time_dedupe(size):
    key = str(size)
    for rep in samples[key]:
        run_on([cmd], samples[key][rep])

time_dedupe.param_names = ['size']
time_dedupe.params      = [sorted([int(s) for s in samples.keys()])]
time_dedupe.timer       = default_timer  # Wall-clock rather than CPU time
