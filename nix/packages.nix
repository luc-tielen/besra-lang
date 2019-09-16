let
  commit = "c90199e913c78918cb2e7fb825c75b1c69ee7464";
  nixpkgs = builtins.fetchTarball {
    name = "nixpkgs-19.09";
    url = "https://github.com/luc-tielen/nixpkgs/archive/${commit}.tar.gz";
    sha256 = "06l03v11603fp7zxaq6bl07s91iigqc4g1lqnxk2gz05cf21xd9y";
  };
  pkgs = import nixpkgs;
in
  pkgs
