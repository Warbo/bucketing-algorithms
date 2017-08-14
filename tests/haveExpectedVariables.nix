defs: with defs;

with rec {
  env = runCommand "theory-env"
    (withNix {
      inherit (quickspecBench.benchVars.standalone)
        genAnnotatedPkg genInput;
      data        = ../benchmarks/list-full.smt2;
      buildInputs = [ jq ];
    })
    ''
      # Convert TIP format to Haskell, extract ASTs and annotate with type info
      RESULT=$("$genAnnotatedPkg" < "$data")

         ASTS=$(echo "$RESULT" | jq -r '.annotated')
      OUT_DIR=$(echo "$RESULT" | jq -r '.out_dir')

      export OUT_DIR

      # Generate a QuickSpec signature for the generated Haskell package, along
      # with the code to explore it.
      "$genInput" < "$ASTS" |
        jq --arg dir "$OUT_DIR" '. + {"out_dir":$dir}' > "$out"
    '';

  code = runCommand "code.hs" { inherit env; buildInputs = [ jq ]; } ''
    jq -r '.code' < "$env" > "$out"
  '';

  # The 'runhaskell' command used to execute the sig generator, run inside the
  # appropriate Nix environment (for GHC, dependencies, generated package, etc.)
  runhaskell = nix-config.wrap {
    name   = "haveExpectedVariables-runhaskell";
    vars   = {
      inherit env;
      NIX_EVAL_HASKELL_PKGS = quickspecBench.customHs;
      NIX_PATH              = quickspecBench.innerNixPath;
      NIX_REMOTE            = "daemon";
    };
    paths  = [ jq nix ];
    script = ''
      #!/usr/bin/env bash
       RUNNER=$(jq -r '.runner'  < "$env")
         EXPR=$(jq -r '.env'     < "$env")
      OUT_DIR=$(jq -r '.out_dir' < "$env")

      export OUT_DIR

      nix-shell --show-trace -p "$EXPR" --run "$RUNNER $*"
    '';
  };

  ticks = "''";

  countVars = writeScript "countVars.hs" ''
    -- TODO: We don't need all of these
    import Test.QuickSpec hiding (lists)
    import Test.QuickSpec.Signature
    import Data.Digest.Murmur32
    import Data.Serialize
    import MLSpec.Helper
    import A
    import IfCxt
    import Test.QuickCheck
    import Test.RuntimeArbitrary
    import Prelude
    import GHC.Types
    import qualified Test.Feat as F

    mkIfCxtInstances ${ticks}F.Enumerable
    mkIfCxtInstances ${ticks}Ord
    mkIfCxtInstances ${ticks}CoArbitrary
    mkIfCxtInstances ${ticks}Arbitrary

    func1 = ("Unary",   Test.RuntimeArbitrary.getArbGen
                          [Prelude.undefined :: ((->) Prelude.Integer
                                                      Prelude.Integer)])
    func2 = ("Binary",  Test.RuntimeArbitrary.getArbGen
                          [Prelude.undefined :: ((->) Prelude.Integer
                                                      ((->) Prelude.Integer
                                                            Prelude.Integer))])
    ints  = ("Integer", Test.RuntimeArbitrary.getArbGen
                          [Prelude.undefined :: (Prelude.Integer)])
    lists = ("List",    Test.RuntimeArbitrary.getArbGen
                          [Prelude.undefined :: ((A.List) Prelude.Integer)])
    nats  = ("Nat",     Test.RuntimeArbitrary.getArbGen
                          [Prelude.undefined :: (A.Nat)])

    check (t, l) = case l of
      [] -> error ("No Arbitrary instance for " ++ t)
      _  -> return ()

    main = sequence [
        check ints
      , check nats
      , check lists
      , check func1
      , check func2
      , putStrLn "Found Arbitrary instances"
      ]
  '';
};
{
  askForVariables = runCommand "ask-for-vars"
    {
      inherit code;
    }
    ''
      echo "Ensuring Haskell code requests variables for appropriate types" 1>&2

      # Find where we're adding variables to the signature and get their types
      # This parsing is pretty fragile; if it breaks, it might be worth using
      # haskell-src-exts or similar.
      TYPES=$(grep -A 2 'MLSpec.Helper.addVars' < "$code" |
              grep 'getArbGen'                            |
              grep -o ':: .*\]'                           |
              grep -o ' .*[^]]'                           |
              grep -o '[^ ].*[^ ]'                        )

      for TYPE in Prelude.Integer '(A.List) Prelude.Integer'
      do
        echo "$TYPES" | grep -F "$TYPE" > /dev/null || {
          echo "Didn't ask for variables of type '$TYPE'" 1>&2
          exit 1
        }
      done

      echo "pass" > "$out"
    '';

  haveGenerators = runCommand "have-generators"
    {
      inherit countVars runhaskell;
    }
    ''
      echo "Checking that we can find Arbitrary instances" 1>&2
      "$runhaskell" < "$countVars"
      echo "pass"   > "$out"
    '';
}