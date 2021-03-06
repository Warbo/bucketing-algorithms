{ bash, coreutils, wrap }:

with builtins;
with rec {
  go = name: bins: wrap ({ inherit name; } //
    (if bins == []
        then { file = "${coreutils}/bin/cat"; }
        else {
               script = ''
                 #!${bash}/bin/bash
                 "${head bins}" | "${go (name + "_") (tail bins)}"
               '';
             }));
};
go
