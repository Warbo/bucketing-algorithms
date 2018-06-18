# Nailgun is a background process for avoiding JVM startup times
{ attrsToDirs, bash, fetchFromGitHub, maven, openjdk, runCommand, weka, wrap }:

with rec {
  version = "0.9.3";

  ngSrc = fetchFromGitHub {
    owner  = "facebook";
    repo   = "nailgun";
    rev    = "604b49b";
    sha256 = "0v04qvx355fliw56d64r2z32nlh9qpfn6abiwxy6aqhhpldpx18l";
  };

  nailgun = runCommand "ng-server"
    {
      inherit ngSrc;
      buildInputs = [ maven ];
    }
    ''
      mkdir "$out"
      cp -r "$ngSrc" "$out/pkg"
      chmod +w -R "$out/pkg"
      cd "$out/pkg"
      mvn package -Dmaven.repo.local="$out/maven-repo"
      make
    '';

  ngServer = wrap {
    name   = "ng-server";
    vars   = {
      CP = builtins.concatStringsSep ":" [
        "${nailgun}/pkg/nailgun-server/target/nailgun-server-0.9.3-SNAPSHOT.jar"
        "${weka}/share/weka/weka.jar"
      ];
    };
    script = ''
      #!/usr/bin/env bash
      exec "${openjdk}/bin/java" -cp "$CP:$CLASSPATH" \
        com.martiansoftware.nailgun.NGServer "$@"
    '';
  };

  ngClient = wrap {
    name = "ng";
    file = "${nailgun}/pkg/ng";
  };
};
attrsToDirs {
  bin = {
    ng          = ngClient;
    withNailgun = wrap {
      name   = "withNailgun";
      paths  = [ bash ];
      vars   = { inherit ngClient ngServer; };
      script = ''
        #!/usr/bin/env bash
        set -e

        if [[ -n "$NAILGUN_PORT" ]]
        then
          echo "Nailgun already running, sticking with that one" 1>&2
          "$@"
          CODE="$?"
          exit "$CODE"
        fi

        # Start nailgun server, daemonise it to background and read off the port
        echo "Starting nailgun server" 1>&2
        coproc "$ngServer" 127.0.0.1:0
        SERVER_OUT="${"$" + "{COPROC[0]}"}"
        SERVER_PID="$COPROC_PID"
        read -ru "${"$" + "{COPROC[0]}"}" first_line

        # Trap exit, so we can shut down the nailgun server (and other children)
        function killNailgun {
          echo "Stopping nailgun server (PID $SERVER_PID)" 1>&2
          kill "$SERVER_PID"
        }
        trap killNailgun EXIT

        NAILGUN_PORT=$(echo "$first_line" | sed -e 's/port 0//g'       |
                                            grep -o 'port [0-9][0-9]*' |
                                            grep -o '[0-9]*')
        export NAILGUN_PORT
        echo "Nailgun started on port $NAILGUN_PORT" 1>&2

        # Run whatever command we've been given
        "$@"
      '';
    };
  };
}
