from timeit import default_timer
from .util  import load_command, load_samples, run_on

cmd     = load_command('getGroundTruths')
samples = load_samples()

def time_groundtruths(size):
    key = str(size)
    for rep in samples[key]:
        run_on([cmd], samples[key][rep])

time_groundtruths.param_names = ['size']
time_groundtruths.params      = [sorted([int(s) for s in samples.keys()])]
time_groundtruths.timer       = default_timer  # Wall-clock rather than CPU time
