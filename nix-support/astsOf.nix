# Command which reads function names from stdin and writes annotated ASTs to
# stdout. We use Template Haskell to build a name->AST mapping ahead of time,
# so lookups don't get slowed down by disk access, parsing, etc.
{ attrsToDirs', fail, haskellPackages, jq, runCommand, tebenchmark, testData,
  withDeps, writeScript }:

with rec {
  modules = {
    "AstsHelpers.hs" = ../haskell-support/AstsHelpers.hs;
    "AstsOf.hs"      = runCommand "AstsOf.hs"
                         {
                           f = ../haskell-support/AstsOf.hs;
                           s = testData.tip-benchmark.asts;
                         }
                         ''sed -e "s@REPLACEME@$s@g" < "$f" > "$out"'';
  };

  script = runCommand "astsOf"
    {
      __noChroot  = true;
      modules     = attrsToDirs' "AstsOfModules" modules;
      buildInputs = [
        (haskellPackages.ghcWithPackages (h: [
          h.aeson h.bytestring h.containers h.text h.th-lift-instances
        ]))
      ];
      main = writeScript "astsof-main.hs" ''
        module Main where
        import qualified AstsOf
        main = AstsOf.main
      '';
    }
    ''
      cp -v "$modules"/*.hs ./
      cp "$main" Main.hs
      ghc --make -O2 -o "$out" Main.hs
    '';

  test = runCommand "test-astsOf"
    {
      inherit script;
      buildInputs = [ fail jq tebenchmark.tools ];
    }
    ''
      S=$(choose_sample 5 10)                    || fail "Didn't sample"
      I=$(echo "$S" | jq -R '.' | jq -s '.')     || fail "Didn't wrap"
      O=$(echo "$I" | "$script")                 || fail "Didn't get ASTs"
      echo "$O" | jq -e 'type | . == "array"'    || fail "$O\nNot object"
      echo "$O" | jq -e 'length | . == 5'        || fail "$O\nNot 5 ASTs"
      echo "$O" | jq --argjson i "$I" \
                     'map(.name) | sort | . == ($i | sort)' ||
        fail "Input:\n$I\n\nOutput:\n$O\n\nMismatching names"
      mkdir "$out"
    '';
};
{
  astsOfModules = modules;
  astsOfScript  = withDeps [ test ] script;
}
