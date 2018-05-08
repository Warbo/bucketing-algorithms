from json       import dumps, loads
from os         import getenv
from subprocess import PIPE, Popen
from sys        import stderr
from timeit     import default_timer

cmd = loads(getenv('commands'))['addHashBucketsCmd']

samples = {}
def parse_samples():
    with open(getenv('samples'), 'r') as f:
        data = loads(f.read())
    for size in data:
        samples[size] = {}
        for rep in data[size]:
            sample = data[size][rep]
            if sample is not None:
                samples[size][rep] = dumps(sample['sample'])

def call_cmd(sample):
    p = Popen([cmd], stdin=PIPE, stdout=PIPE, stderr=PIPE)
    p.communicate(sample.encode('utf-8'))

def time_hashing():
    for size in samples:
        for rep in samples[size]:
            call_cmd(samples[size][rep])

time_hashing.setup = parse_samples
time_hashing.timer = default_timer  # Measure wall-clock rather than CPU time
