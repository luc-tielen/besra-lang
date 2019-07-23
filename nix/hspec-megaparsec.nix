let
  url = "https://github.com/luc-tielen/hspec-megaparsec.git";
  ref = "master";
  rev = "95ac2b9c49426cf24b2a4fef3128753405feb915";
  repo = builtins.fetchGit { inherit url ref rev; };
in
  repo
