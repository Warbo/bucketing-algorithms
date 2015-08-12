#! /usr/bin/env nix-shell
#! nix-shell -p treefeatures -p bash -p jq -i bash

jq -c '.[]' |
    while read -r LINE
    do
        # Extract the "ast" value and pipe into TreeFeatures
        FEATURES=$(echo "$LINE" | jq -r '.ast' | BITS=30 MODE=sexpr TreeFeatures)

        # Add the features to the object
        echo "$LINE" | jq -c ". + {features: \"$FEATURES\"}"
    done | jq -s '.'
