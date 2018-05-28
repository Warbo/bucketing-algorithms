from json       import dumps, loads
from os         import getenv
from subprocess import PIPE, Popen

def load_samples():
    samples = {}
    with open(getenv('samples'), 'r') as f:
        data = loads(f.read())
    for size in data:
        samples[size] = {}
        for rep in data[size]:
            sample = data[size][rep]
            if sample is not None:
                samples[size][rep] = dumps(sample['sample'])
    return samples

def load_command(c):
    return loads(getenv('commands'))[c]

def run_on(cmd, stdin):
    p = Popen(cmd, stdin=PIPE, stdout=PIPE, stderr=PIPE)
    return p.communicate(stdin.encode('utf-8'))
