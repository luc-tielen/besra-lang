{ compiler ? "ghc864", pkgs ? import ./packages.nix {} }:

(import ./. { inherit pkgs compiler; }).besra-shell
