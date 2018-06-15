import json
from timeit import default_timer
from .util  import load_command, load_samples, run_on

cmd     = load_command('getGroundTruths')
samples = load_samples()

def time_groundtruths(size):
    key = str(size)
    for rep in samples[key]:
        data = json.loads(samples[key][rep])
        run_on([cmd], json.dumps({"data": data}))

time_groundtruths.param_names = ['size']
time_groundtruths.params      = [sorted([int(s) for s in samples.keys()])]
time_groundtruths.timer       = default_timer  # Wall-clock rather than CPU time
