# ML4HSFE executable from its Haskell package, with its test script checked
{ haskellPackages, jq, runCommand, withDeps }:

with {
  test = runCommand "ML4HSFE-test-suite"
    {
      ml4hsfe     = haskellPackages.ML4HSFE.src;
      buildInputs = [
        (haskellPackages.ghcWithPackages (h: [
          h.cabal-install h.ghc h.kmeans h.ML4HSFE h.quickspec h.tasty
          h.tasty-quickcheck
        ]))
        jq
      ];
    }
    ''
      set -e
      export HOME="$PWD"
      cp -r "$ml4hsfe" ./ml4hsfe
      chmod +w -R ./ml4hsfe
      cd ./ml4hsfe
      bash ./test.sh
      mkdir "$out"
    '';
};
withDeps [ test ] haskellPackages.ML4HSFE
