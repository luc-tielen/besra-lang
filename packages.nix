let
  nixpkgs = builtins.fetchTarball {
    name = "nixos-18.09";
    url = https://github.com/nixos/nixpkgs/archive/18.09.tar.gz;
    sha256 = "1ib96has10v5nr6bzf7v8kw7yzww8zanxgw2qi1ll1sbv6kj6zpd";
  };
  #pkgs = import nixpkgs;
  pkgs = import <nixpkgs>;
in
  pkgs
