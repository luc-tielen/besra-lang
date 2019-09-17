let
  commit = "6f120bbe8427e374f17dcc361be231c9da85f51b";
  nixpkgs = builtins.fetchTarball {
    name = "nixpkgs-19.09";
    url = "https://github.com/luc-tielen/nixpkgs/archive/${commit}.tar.gz";
    sha256 = "19c38dk9g0a80pbnkd96lw9d68k3dmakib7gwby5ik1wd31vm2i2";
  };
  pkgs = import nixpkgs;
in
  pkgs
