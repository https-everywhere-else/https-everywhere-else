{ nixpkgs ? import <nixpkgs> }:

let
  snapshot = "lts-11.14";
  stackage = import (fetchTarball {
    url = "https://stackage.serokell.io/4ksix2mhabvirycmmdn7n892gf1hkhfc-stackage/default.nix.tar.gz";
    sha256 = "0hvbf51m0ci41gd6cyffz5vm3308w6nhp7l7cj3f5i04zn0dr8lq";
  });
  pkgs = nixpkgs { overlays = [ stackage."${snapshot}" ]; };

in
  (pkgs.haskellPackages.override (with pkgs.haskell.lib; {
    overrides = final: previous: {
      http-proxy = dontCheck (doJailbreak previous.http-proxy);
      https-everywhere-rules = final.callCabal2nix "https-everywhere-rules" ./. {};
    };
  })).https-everywhere-rules
