from timeit import default_timer
from .util  import load_command, load_samples, run_on

cmd     = load_command('addHashBucketsCmd')
samples = load_samples()

def time_hashing(size):
    key = str(size)
    # We deterministically choose a single rep for now, to avoid timing out
    for rep in samples[key]:
        run_on([cmd], samples[key][rep])

time_hashing.param_names = ['size']
time_hashing.params      = [sorted([int(s) for s in samples.keys()])]
time_hashing.timer       = default_timer  # Wall-clock rather than CPU time
