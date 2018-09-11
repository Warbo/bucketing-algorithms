# Scripts for bucketing samples in a variety of ways and measure the
# proportion of ground truth theorems which apply to the resulting buckets.
#
# Write output to JSON for archiving.
{ attrsToDirs', averageProportions, benchmarkingCommands, calculateProportions,
  callPackage, ghcWithML4HSFE, ghostscript, haskellPackages, makeSamples,
  moreutils, runCommand, wrap, writeScript }:

with { inherit (builtins) concatStringsSep map; };
with callPackage ./astsOf.nix {};
with rec {
  # Runs each sample through all bucketers, adding the result to the samples
  # JSON
  addBuckets = { profile ? false }: wrap {
    name  = "process-samples${if profile then "prof" else ""}";
    vars  = { LANG = "en_US.UTF-8"; };
    file  = runCommand "process-samples-script${if profile then "prof" else ""}"
      {
        buildInputs = [ (ghcWithML4HSFE { inherit profile; }) ];
        mods        = attrsToDirs' "bucket-proportions-mods" (astsOfModules // {
          "BucketUtil.hs"      = ../haskell-support/BucketUtil.hs;
          "HashBucket.hs"      = ../haskell-support/HashBucket.hs;
          "RecurrentBucket.hs" = ../haskell-support/RecurrentBucket.hs;
          "Main.hs"            = writeScript "process-samples-main.hs" ''
            module Main where

            import qualified AstsOf
            import qualified BucketUtil
            import qualified HashBucket
            import qualified RecurrentBucket

            bucketers = [HashBucket.bucketer, RecurrentBucket.bucketer]

            main = BucketUtil.bucketStdio bucketers AstsOf.astsOf'
          '';
        });
      }
      ''
        cp -rv "$mods" mods
        chmod +w -R mods
        cd mods
        ghc --make ${if profile then "" else "-O2"} -o "$out" Main.hs
        ${if profile
             /* Only compile for profiling after we've done a normal compile, so
                GHC finds unprofiled TemplateHaskell snippets it can run (it
                can't run/splice profiled ones). Use osuf to keep them separate.
                https://downloads.haskell.org/%7Eghc/6.10.2/docs/html/users_guide/template-haskell.html#id2715975
              */
             then "ghc --make -o \"$out\" -prof -fprof-auto -osuf p_o Main.hs"
             else ""}
      '';
  };
};

{
  inherit averageProportions calculateProportions;
  inherit (benchmarkingCommands) getGroundTruths;
  addBuckets = addBuckets {};

  # Useful for tracking down misbehaving bucketers, etc.
  memoryProfile = runCommand "bucketing-memory-profile"
    {
      prog    =  addBuckets { profile = true;          };
      samples = makeSamples { maxSize = 10; reps = 10; };
    }
    ''
      function getStderr {
        echo "BEGIN"
        "$prog" +RTS -hc -L50 -RTS < "$samples" 2>&1 1> /dev/null
        echo "END"
      }

      mkdir "$out"
      cd "$out"
      getStderr | "${moreutils}/bin/ts" -s "%T" | tee times
      mv -v *.hp heap.hp

      echo "Rendering heap usage as PostScript chart" 1>&2
      if "${ghcWithML4HSFE {}}/bin/hp2ps" -c < heap.hp > heap.ps
      then
        echo "Converting PostScript to PDF" 1>&2
        "${ghostscript}/bin/ps2pdf" heap.ps ||
          echo "WARNING: Failed converting chart to PDF" 1>&2
      else
        echo "WARNING: Failed to render PostScript heap chart" 1>&2
      fi

      echo "Rendering heap usage as SVG chart" 1>&2
      "${haskellPackages.hp2pretty}/bin/hp2pretty" heap.hp ||
        echo "WARNING: Failed to render SVG heap chart"
    '';
}
