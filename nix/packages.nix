let
  commit = "10c7d9666378b514c20ec481ad2ccc6289543606";
  nixpkgs = builtins.fetchTarball {
    name = "nixpkgs-19.09";
    url = "https://github.com/luc-tielen/nixpkgs/archive/${commit}.tar.gz";
    sha256 = "0zyfn7qayn2xh47p0qbw523mjm96qyk7416y7jrn3s14wdxskl01";
  };
  pkgs = import nixpkgs;
in
  pkgs
