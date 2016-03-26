{ stdenv, jq, getDeps, utillinux }:

stdenv.mkDerivation {
  name = "annotatedb";

  # Exclude .git and test-data from being imported into the Nix store
  src = builtins.filterSource (path: type:
    baseNameOf path != ".git" &&
    baseNameOf path != "test-data") ./.;

  propagatedBuildInputs = [ jq getDeps utillinux ];

  NIX_REMOTE = "daemon";
  NIX_PATH   = builtins.getEnv "NIX_PATH";
  doCheck    = true;
  checkPhase = ''
    ./test.sh
  '';

  installPhase = ''
    mkdir -p "$out/bin"
    for FILE in annotateAsts annotateDb getArities getDeps getTypes runTypes tagAsts
    do
        cp -v "$FILE" "$out/bin/"
        chmod +x "$out/bin"
    done
  '';
}
