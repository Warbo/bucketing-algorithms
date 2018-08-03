from timeit import default_timer
from .util  import load_command, run_on

cmd = load_command('sample')

def time_sample(size):
    for reps in [1, 10, 100]:
        run_on([cmd, str(size), str(reps)], "")

time_sample.param_names = ['size']
time_sample.params      = [[1, 10, 20, 50, 100]]
time_sample.timer       = default_timer  # Wall-clock rather than CPU time
