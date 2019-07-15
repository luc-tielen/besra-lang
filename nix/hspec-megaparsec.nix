let
  url = "https://github.com/luc-tielen/hspec-megaparsec.git";
  ref = "better-hspec-output";
  rev = "0fdda8b9a5821b8a66a672edd897ae21bccf78f9";
  repo = builtins.fetchGit { inherit url ref rev; };
in
  repo
