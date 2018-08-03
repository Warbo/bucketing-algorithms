from timeit import default_timer
from .util  import load_command, run_on

cmd = load_command('sample')

def time_sample():
    run_on([cmd], "")

time_sample.timer = default_timer  # Wall-clock rather than CPU time
