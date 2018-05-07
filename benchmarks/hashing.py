from json       import dumps, loads
from os         import getenv
from parameters import reps, sizes
from subprocess import PIPE, Popen
from timeit     import default_timer
from util       import tip_benchmarks, tip_cache

cmd = loads(os.getenv('commands'))['addHashBucketsCmd']

samples = None
def parse_samples():
    with open(os.getenv('samples'), 'r') as f:
        samples = loads(f.read())

def call_cmd(sample):
    p = Popen([cmd], stdin=PIPE, stdout=PIPE, stderr=PIPE)
    p.communicate(dumps(sample))

def time_hashing():
    for size in samples:
        for rep in samples[size]:
            call_cmd(samples[size][rep]['sample'])
time_hashing.setup = parse_samples
time_hashing.timer = default_timer  # Measure wall-clock rather than CPU time
