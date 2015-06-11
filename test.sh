#!/bin/sh

RESULT=""

for pkg in hipspecifyer haskell-src-exts hipspec QuickCheck structural-induction
do
    for ghc in 784 7101
    do
        RESULT="$RESULT\nTesting $pkg $ghc: "
        if nix-shell -p "(import ./. haskell.packages.ghc${ghc}).$pkg" \
                     --command 'true'
        then
            RESULT="$RESULT PASS"
        else
            RESULT="$RESULT FAIL"
        fi
        echo -e "$RESULT"
    done
done
