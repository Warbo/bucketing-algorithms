from os import getenv, listdir
from os.path import isfile, join

def contents(*args):
    return listdir(join(*args))

def track_issues():
    issues = join(getenv('root'), '.issues')
    count  = 0
    for i in contents(issues):
        fixed = False
        # Loop through new, cur and tmp
        for d in contents(issues, i):
            # One message will be the "root" with metadata stored in its headers
            for msg in contents(issues, i, d):
                content = ''
                with open(join(issues, i, d, msg), 'r', encoding='utf-8') as f:
                    content = f.read()
                for line in content.split('\n'):
                    if line.startswith('resolution:') and 'fixed' in line:
                        fixed = True
        if not fixed:
            count += 1

    return count

track_issues.unit = 'issues'
