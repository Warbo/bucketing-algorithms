# Set 'bypassPublicApi' to get access to all of our implementation details, but
# keep in mind that we make no guarantees about their stability.
{ args ? {}, bypassPublicApi ? false }:

with {
  defs = rec {
    # Implementation details
    pkgs = import ./nix-support args;

    # Used for general performance testing, as well as formal evaluation
    benchmarkRunner = import ./shell.nix;

    # Provides our exploration scripts
    inherit (pkgs) package;
  };
};
if bypassPublicApi
   then defs
   else defs.package
