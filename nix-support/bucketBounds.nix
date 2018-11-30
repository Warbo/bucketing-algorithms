{ attrsToDirs', ghcWithML4HSFE, lib, nixpkgs1703, nixpkgs1803, runCommand,
  writeScript }:

with builtins;
with lib;
with rec {
  renderModel = { bound, bucketSize, names, theorems }:
    with rec {
        nameCount      = length names;
      bucketCount      = (nameCount / bucketSize) + 1;
      bucketRemainder  =  nameCount - ((bucketCount - 1) * bucketSize);

        nameCountS     = toString   nameCount;
      bucketCountS     = toString bucketCount;
      bucketRemainderS = toString bucketRemainder;
      bucketSizeS      = toString bucketSize;

      type   = "set of 1.." + nameCountS;
      bucket = n: "bucket" + toString n;

      # Since we models the names as numbers 1..nameCount, we convert the deps
      # of each theorem into their corresponding numbers too.
      deps =
        with {
          indices = listToAttrs (imap1 (value: name: {
                                         inherit name value;
                                       })
                                       names);
        };
        imap1 (i: ds: {
                id   = "theorem" + toString i;
                deps = map (n: getAttr n indices) ds;
              })
              (attrValues theorems);
    };
    writeScript "bounds.mzn" ''
      include "globals.mzn";

      % Declare bucketCount buckets, as sets of elements from 1 to nameCount
      ${concatStringsSep "\n"
          (map (n: "var ${type}: ${bucket n};")
               (range 1 bucketCount))}

      % Force bucket sizes: bucket1 will be the remainder (which may be zero),
      % all others are of size bucketSize
      constraint card(bucket1) = ${bucketRemainderS};
      ${concatStringsSep "\n"
          (map (n: "constraint card(${bucket n}) = ${bucketSizeS};")
               (range 2 bucketCount))}

      % Buckets should not overlap
      ${concatStringsSep "\n"
          (map (x: concatStringsSep "\n"
                     (map (y: if x >= y
                                 then ""
                                 else with rec {
                                        bx   = "bucket${toString x}";
                                        by   = "bucket${toString y}";
                                        expr = "${bx} intersect ${by}";
                                      };
                                      "constraint card(${expr}) = 0;")
                          (range 1 bucketCount)))
               (range 1 bucketCount))}

      ${concatStringsSep "\n"
          (map (theorem: concatStrings [
                 "function var int: "
                 theorem.id "(var ${type}: bucket) = "
                 "if (" (concatStringsSep " /\\ "
                           (map (d: "(${toString d} in bucket)")
                                theorem.deps)) ") "
                   "then 1 "
                   "else 0 "
                 "endif;"
               ])
               deps)}

      function var int: score(var ${type}: bucket) = ${
        concatStringsSep " + "
          (map (theorem: "(" + theorem.id + "(bucket))")
               deps)
      };

      solve ${bound}imize ${concatStringsSep " + "
                              (map (n: "score( ${bucket n} )")
                                   (range 1 bucketCount))};

      array[1..${nameCountS}] of string: names;
      names = [ ${concatStringsSep ", " (map (n: ''"${n}"'') names)} ];

      output [${concatStringsSep '', "\n", ''
                  (map (n: ''show([ names[i] | i in ${bucket n} ])'')
                       (range 1 bucketCount))}];
    '';

  sampleBounds = sample:
    with {
      args = {
        bucketSize = 5;
        names      = [ "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" ];
        theorems   = {
          t1 = [ "a" "b" "i" ];
          t2 = [ "a" "g"     ];
          t3 = [ "d" "j" "k" ];
        };
      };
    };
    runCommand "bucket-bounds"
      {
        buildInputs = [ nixpkgs1703.gecode nixpkgs1803.minizinc ];
        model       = renderModel ;
      }
      ''
        echo "Solving $model" 1>&2
        mzn-fzn -f fzn-gecode "$model" > "$out"
      '';

  bucketBoundBuilder = { cmds ? [], deps ? {}, main }:
    runCommand "bucket-bound-runner-${main}"
      (deps // {
        buildInputs = [
          (ghcWithML4HSFE { extraPkgs = [ "lens-aeson" ]; })
        ];
        src         = attrsToDirs' "bucket-bound-src" {
          "BucketBounds.hs" = ../haskell-support/BucketBounds.hs;
          "BucketUtil.hs"   =  ../haskell-support/BucketUtil.hs;
          "Main.hs"         = writeScript "bb-main.hs" ''
            module Main where
            import qualified BucketBounds
            main = BucketBounds.${main}
          '';
        };
      })
      ''
        ${concatStringsSep "\n" cmds}
        cp -v "$src"/* ./
        ghc --make -O2 -o "$out" Main
      '';

  bucketBoundRunner = bucketBoundBuilder {
    cmds = [ "$tests" ];
    deps = {
      tests = bucketBoundBuilder {
        main = "boundsTest";
      };
    };
    main = "boundsMain";
  };
};
bucketBoundRunner
