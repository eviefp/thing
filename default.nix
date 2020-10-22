let
  sources = import ./nix;
  tooling = sources.tooling;
  pkgs = tooling.pkgs;
  gis = sources.gis;
in
  pkgs.haskell.packages.ghc883.callCabal2nix "learn-brick" (gis.gitignoreSource ./.) {}
