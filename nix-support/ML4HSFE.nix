# ML4HSFE executable from its Haskell package, with its test script checked
{ haskellPackages, jq, runCommand, withDeps }:

rec {
  def   = haskellPackages.ML4HSFE;
  tests = runCommand "ML4HSFE-test-suite"
    {
      ml4hsfe     = def.src;
      buildInputs = [
        (haskellPackages.ghcWithPackages (h: [
          h.cabal-install h.ghc h.kmeans h.ML4HSFE h.quickspec h.tasty
          h.tasty-quickcheck
      ]))
      jq
    ];
    }
  ''
    export HOME="$PWD"
    cp -r "$ml4hsfe" ./ml4hsfe
    chmod +w -R ./ml4hsfe
    cd ./ml4hsfe
    bash ./test.sh
    mkdir "$out"
  '';
}
