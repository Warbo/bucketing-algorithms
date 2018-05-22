from json       import dumps, loads
from os         import getenv
from subprocess import PIPE, Popen
from sys        import stderr
from timeit     import default_timer

cmd = loads(getenv('commands'))['addHashBucketsCmd']

samples = {}
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

def time_hashing(size):
    key = str(size)
    for rep in samples[key]:
        stderr.write('Running rep {0}\n'.format(str(rep)))
        call_cmd(samples[key][rep])
        break

time_hashing.param_names = ['size']
time_hashing.params      = [sorted([int(s) for s in samples.keys()])]
time_hashing.timer       = default_timer  # Wall-clock rather than CPU time
