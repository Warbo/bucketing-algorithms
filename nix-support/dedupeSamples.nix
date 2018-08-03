{ python3, wrap }:

with {
  go = wrap {
    name   = "dedupe.py";
    paths  = [ python3 ];
    script = ''
      #!/usr/bin/env python3
      import json
      import sys

      data = json.loads(sys.stdin.read())
      for size in data:
        seen = []
        for iRep in sorted([int(rep) for rep in data[size]]):
          rep = str(iRep)
          sample = frozenset(data[size][rep])
          if sample in seen:
            data[size][rep] = None
          else:
            data[size][rep] = {'sample': data[size][rep]}
          seen += [sample]
      print(json.dumps(data))
    '';
  };
};
go
