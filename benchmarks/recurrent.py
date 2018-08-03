from timeit import default_timer
from .util  import load_command, load_samples, run_on

cmd     = load_command('addRecurrentBucketsCmd')
samples = load_samples()

def time_recurrent(size):
    key = str(size)
    for rep in samples[key]:
        run_on([cmd], samples[key][rep])

time_recurrent.param_names = ['size']
time_recurrent.params      = [sorted([int(s) for s in samples.keys()])]
time_recurrent.timer       = default_timer  # Wall-clock rather than CPU time
