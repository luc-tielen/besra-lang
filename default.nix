{ compiler ? "ghc864", pkgs ? import ./packages.nix {} }:

with pkgs;

let
  haskellPkgs = haskell.packages.${compiler};
  source = nix-gitignore.gitignoreSource [] ./.;
  drv = haskellPkgs.callCabal2nix "x1" source {};
in
  {
    x1 = drv;
    x1-shell = haskellPkgs.shellFor {
      packages = p: [ drv ];
      buildInputs = with haskellPkgs; [
        neovim
        pkgs.git
        pkgs.less
        pkgs.gnupg
        cabal-install
        hpack
        hlint
        ghcid
      ];
      withHoogle = true;
    };
  }
