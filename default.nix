{ compiler ? "ghc864", pkgs ? import ./packages.nix {} }:

with pkgs;

let
  haskellPkgs = haskell.packages.${compiler};
  source = nix-gitignore.gitignoreSource [] ./.;
  drv = haskellPkgs.callCabal2nix "besra" source {};
in
  {
    besra = drv;
    besra-shell = haskellPkgs.shellFor {
      packages = p: [ drv ];
      buildInputs = with haskellPkgs; [
        cabal-install
        hpack
        hlint
        ghcid
      ];
      withHoogle = true;
    };
  }
