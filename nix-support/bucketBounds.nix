{ lib, minizinc, nixpkgs1703, runCommand, writeScript }:

with builtins;
with lib;
with {
  renderModel = { bucketSize, names, theorems }:
    with rec {
        nameCount      = length names;
      bucketCount      = (nameCount / bucketSize) + 1;
      bucketRemainder  =  nameCount - ((bucketCount - 1) * bucketSize);

        nameCountS     = toString   nameCount;
      bucketCountS     = toString bucketCount;
      bucketRemainderS = toString bucketRemainder;
      bucketSizeS      = toString bucketSize;

      set = "1.." + nameCountS;
    };
    writeScript "bounds.mzn" ''
      include "globals.mzn";

      % Declare bucketCount buckets, as sets of elements from 1 to nameCount
      ${concatStringsSep "\n"
          (map (n: "var set of ${set}: bucket${toString n};")
               (range 1 bucketCount))}

      % Force bucket sizes: bucket1 will be the remainder (which may be zero),
      % all others are of size bucketSize
      constraint card(bucket1) = ${bucketRemainderS};
      ${concatStringsSep "\n"
          (map (n: "constraint card(bucket${toString n}) = ${bucketSizeS};")
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

      % TODO: Fix score
      function var int: score(var set of ${set}: bucket) = min(bucket);

      solve maximize ${concatStringsSep " + "
                         (map (n: "score( bucket${toString n} )")
                              (range 1 bucketCount))};

      output [ ${concatStringsSep " ++ "
                   (map (n: ''"[" ++ show( bucket${toString n} ) ++ "]"'')
                        (range 1 bucketCount))} ];
    '';
};
/*{ samples }:*/ runCommand "bucket-bounds"
  {
    buildInputs = [ nixpkgs1703.gecode minizinc ];
    model       = renderModel {
      bucketSize = 5;
      names      = [ "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" ];
      theorems   = {
        t1 = [ "a" "b" "i" ];
        t2 = [ "a" "g"     ];
        t3 = [ "d" "j" "k" ];
      };
    };
  }
  ''
    echo "Solving $model" 1>&2
    #cp "$model" model.mzn
    #mzn2fzn model.mzn
    #cat model.fzn
    mzn-fzn -f fzn-gecode "$model" > "$out"
  ''
