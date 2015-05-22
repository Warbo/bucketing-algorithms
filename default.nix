with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "ml4hs";
  src  = ./.;
  buildInputs = [
    hs2ast
    treefeats
    weka
    openjre
  ];

  shellHook = ''
    # -jar weka.jar launches the GUI, -cp weka.jar runs from CLI
    function weka-cli {
      ${openjre}/bin/java -Xmx1000M -cp ${weka}/share/weka/weka.jar "$@"
    }
  '';
}
