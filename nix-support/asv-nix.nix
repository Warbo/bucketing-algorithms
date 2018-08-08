{ fetchFromGitHub, nix-helpers }:

with {
  src = fetchFromGitHub {
    owner  = "Warbo";
    repo   = "asv-nix";
    rev    = "54d2a89";
    sha256 = "0hh56xk8z1bzv2v1j2vxmmap8bww8wkjkfqx4cf43jgigalw5miz";
  };
};
import "${src}" { path = nix-helpers.repo1803; }
