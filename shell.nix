# Runs the benchmarks with some default options. Can also be used with nix-shell
# to enter a shell suitable for custom benchmark invocations.
with builtins;
with import ./.;
with {
  asv-nix = callPackage ./benchmarks/asv-nix.nix {};
  fixHtml = callPackage ./benchmarks/fixHtml.nix {};
};
runCommand "haskell-te-benchmark"
  (withNix {
    buildInputs  = [ asv-nix fixHtml nixpkgs1609.git nixpkgs1609.rsync ];

    existing     = import ./benchmarks/env.nix {
      dir  = ./.;
      root = ./.;
    };
    msg = ''
      Benchmarking shell: use the 'asv' command to run benchmarks. If you want
      to avoid building environments for each git revision, and instead use a
      single environment based on this directory's contents (as they were when
      'nix-shell' was called, at least) then you can give 'asv run' the option
      '-E "existing:$existing/bin/python". This is especially useful if we don't
      have a .git directory (e.g. if this directory was obtained using the
      'fetchgit' or 'fetchFromGitHub' Nix functions).

      By default, the HTML generated by 'asv publish' won't work when accessed
      via 'file://' URLs. To fix this, you can run 'fixHtml /path/to/html/dir'.
    '';
    shellHook = "echo \"$msg\" 1>&2";
    source    = ./.;
  })
  ''
    export HOME="$PWD/home"
    mkdir "$HOME"

    echo "Copying '$source', in order to benchmark it" 1>&2
    rsync -r --exclude .git        \
             --exclude __pycache__ \
             --exclude .asv        \
             --exclude .issues     \
             "$source"/ ./src
    chmod +w -R ./src
    cd ./src

    # We don't copy .git, since Nix might have removed it anyway (for
    # reproducibility), so we only benchmark the checked-out revision.

    # Real values taken from a Thinkpad X60s
    echo "Generating machine config" 1>&2
    asv machine --arch    "i686"                                            \
                --cpu     "Genuine Intel(R) CPU           L2400  @ 1.66GHz" \
                --machine "default"                                         \
                --os      "Linux 4.4.52"                                    \
                --ram     "3093764"

    echo "Run all benchmarks in pre-build environment" 1>&2
    asv run --show-stderr --machine default \
                          --environment "existing:$existing/bin/python"

    echo "Generating HTML reports" 1>&2
    asv publish --environment "existing:$existing/bin/python"

    # Make reports work as standalone files (no HTTP server, no 3rd-party stuff)
    fixHtml .asv/html

    mkdir "$out"
    cp -r .asv/html    "$out/"
    cp -r .asv/results "$out/"
  ''
