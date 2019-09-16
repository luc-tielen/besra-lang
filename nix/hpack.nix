let
  url = "https://github.com/InBetweenNames/hpack.git";
  ref = "mine-pristine";
  rev = "86791f4206633936f3ada626ec0bdbe363a20cc9";
  repo = builtins.fetchGit { inherit url ref rev; };
in
  repo
