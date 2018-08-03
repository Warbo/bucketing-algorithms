#!/usr/bin/env bash
set -e

function filesSuffixed {
    find . -name "*$1" | grep -v '/\.asv/env/'
}

# Simple, quick sanity check. Useful as a git pre-commit hook.

filesSuffixed ".nix" | while read -r F
do
    echo "Checking syntax of '$F'" 1>&2
    nix-instantiate --parse "$F" > /dev/null
done

filesSuffixed ".py" | while read -r F
do
    echo "Checking syntax of '$F'" 1>&2
    python -m py_compile "$F"
done
