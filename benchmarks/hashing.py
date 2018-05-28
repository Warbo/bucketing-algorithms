from json       import dumps, loads
from os         import getenv
from subprocess import PIPE, Popen
from sys        import stderr
from timeit     import default_timer
from .util      import load_command, load_samples, run_on

cmd     = load_command('addHashBucketsCmd')
samples = load_samples()

def time_hashing(size):
    key = str(size)
    for rep in samples[key]:
        stderr.write('Running rep {0}\n'.format(str(rep)))
        run_on([cmd], samples[key][rep])

time_hashing.param_names = ['size']
time_hashing.params      = [sorted([int(s) for s in samples.keys()])]
time_hashing.timer       = default_timer  # Wall-clock rather than CPU time
