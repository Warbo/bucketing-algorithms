import json
from timeit import default_timer
from .util  import load_bucketed, load_command, run_on

cmd      = load_command('getGroundTruths')
bucketed = load_bucketed();

def time_groundtruths():
    run_on([cmd], json.dumps(bucketed))

time_groundtruths.timer = default_timer  # Wall-clock rather than CPU time
