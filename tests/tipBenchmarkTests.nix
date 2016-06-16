defs: with defs;

testMsg (parseJSON (runScript { buildInputs = [ cabal-install ]; } ''
   pushd "${tipBenchmarks.path}"
   if ./test.sh
   then
     echo "true" > "$out"
   else
     echo "false" > "$out"
   fi
'')) "Tip benchmark tests pass"