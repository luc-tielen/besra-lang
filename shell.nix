{ compiler ? "ghc863", pkgs ? import ./packages.nix {} }:

(import ./. { inherit pkgs compiler; }).x1-shell
