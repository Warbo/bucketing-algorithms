{ minizinc, nixpkgs1703, runCommand, writeScript }:

with builtins;
/*{ samples }:*/ runCommand "bucket-bounds"
  {
    buildInputs = [ nixpkgs1703.gecode minizinc ];
    example = writeScript "example.mzn" ''
      % Colouring Australia using nc colours
      int: nc = 3;

      ${concatStringsSep "\n" (map (region: "var 1..nc: ${region};")
                                   [ "wa" "nt" "sa" "q" "nsw" "v"  "t" ])}

      ${concatStringsSep "\n"
          (map (rs: "constraint ${concatStringsSep " != " rs};")
               [ [ "wa"  "nt"  ] [ "wa" "sa"  ]
                 [ "nt"  "sa"  ] [ "nt" "q"   ]
                 [ "sa"  "q"   ] [ "sa" "nsw" ] [ "sa" "v" ]
                 [ "q"   "nsw" ]
                 [ "nsw" "v"   ] ])}

      solve satisfy;
      output [${concatStringsSep ", "
                  (map (r: ''"${r}=", show(${r}), "\n"'')
                       [ "wa" "nt" "sa" "q" "nsw" "v"  "t" ])}];
    '';
  }
  ''
    mzn-fzn -f fzn-gecode "$example" > "$out"
  ''
