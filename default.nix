{ compiler ? "ghc881", pkgs ? import ./nix/packages.nix {} }:

with pkgs;

let
  hspec-megaparsec = import ./nix/hspec-megaparsec.nix;
  haskellPackages = haskell.packages.${compiler};
  haskellPkgs = haskellPackages.override {
    overrides = self: super: {
      #hspec-megaparsec = self.callCabal2nix "hspec-megaparsec" hspec-megaparsec {};
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
        #cabal-install
        #hpack
        #hlint
        #ghcid
      ];
      #withHoogle = true;
    };
  }
