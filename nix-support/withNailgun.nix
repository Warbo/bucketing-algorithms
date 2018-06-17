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

        # Start nailgun server, daemonise it to background and read off the port
        read -r first_line < <("$ngServer" 127.0.0.1:0 &)
        NAILGUN_PORT=$(echo "$first_line" | sed -e 's/port 0//g'       |
                                            grep -o 'port [0-9][0-9]*' |
                                            grep -o '[0-9]*')
        export NAILGUN_PORT

        # Trap exit, so we can shut down the nailgun server
        function ngStop {
          "$ngClient" ng-stop
        }
        trap ngStop EXIT

        # Run whatever command we've been given
        "$@"
      '';
    };
  };
}
