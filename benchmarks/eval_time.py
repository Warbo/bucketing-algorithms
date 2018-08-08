from timeit import default_timer
from .util  import load_command, run_on

cmd = load_command('evaluator')

def time_eval():
    run_on([cmd], "")

time_eval.timer   = default_timer  # Wall-clock rather than CPU time
time_eval.timeout = 3600.0
