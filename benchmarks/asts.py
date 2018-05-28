from json       import dumps, loads
from os         import getenv
from sys        import stderr
from timeit     import default_timer
from .util      import load_command, load_samples, run_on

cmd     = load_command('astsOf')
samples = load_samples()

def time_asts(size):
    key = str(size)
    for rep in samples[key]:
        run_on([cmd], samples[key][rep])

time_asts.param_names = ['size']
time_asts.params      = [sorted([int(s) for s in samples.keys()])]
time_asts.timer       = default_timer  # Wall-clock rather than CPU time
