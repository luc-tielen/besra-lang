{ compiler ? "ghc881", pkgs ? import ./nix/packages.nix {} }:

with pkgs;

let
  haskellPackages = haskell.packages.${compiler};
  hpack = haskellPackages.callPackage ./nix/hpack.nix {};
  haskellPkgs = haskellPackages.override {
    overrides = self: super: {
      hpack = self.callCabal2nix "hpack" hpack {};
    };
  };
  source = nix-gitignore.gitignoreSource [] ./.;
  #drv = haskellPkgs.callCabal2nix "besra" source {};
in
  {
    #besra = drv;
    besra-shell = haskellPkgs.shellFor {
      #packages = p: [ drv ];
      packages = p: [];
      buildInputs = with haskellPkgs; [
        cabal-install
        hpack
        hlint
        ghcid
      ];
      withHoogle = true;
    };
  }
