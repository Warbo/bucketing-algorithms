# Fetches benchmark run results from git and extracts the samples from them.
with import <nixpkgs> {};
runCommand "samples.json"
  {
    buildInputs = [ jq lzip ];

    # jq expression for digging into ASV benchmark results to get the input data
    # and map over each size and rep, to get only the samples.
    expr        = ''
      .results |
      .["quickspectip.track_data"] |
      .result                      |
      .[0]                         |
      map_values(.reps | map_values({"sample" : .sample}))
    '';

    # ASV benchmark data is stored in this repo
    repo = fetchgit {
      url    = http://chriswarbo.net/git/haskell-te.git;
      rev    = "8904b29";
      sha256 = "059h0jh01ay1kw2l0wh2i3sqni8bc4k3ankrh7cf0v624knbqb09";
    };

    # The benchmarks were run in three parts, which we need to merge together
    resultFiles = [
      # We originally only ran on even sample sizes, to halve the running time
      "b1247807"

      # The evens finished faster than expected, so we ran the odds too
      "bdea634a"

      # The SpeedUp Test procedure treats 30 samples as "small", so we ran each
      # size again to get a 31st sample
      "a531ce8f"
    ];
  }
  ''
    # Start without any sizes
    echo '{}' > samples.json

    # Accumulate sizes from each result file
    for RESULT in $resultFiles
    do
      F="$repo/benchmarks/results/desktop/$RESULT-nix-py-dirnull.json.lz"

      echo "Extracting data from '$F'" 1>&2
      lzip -d < "$F" | jq "$expr" > new.json

      echo "Merging data into result" 1>&2
      jq --argfile old samples.json '. * $old' < new.json > result.json

      rm new.json
      mv result.json samples.json
    done

    mv samples.json "$out"
  ''
