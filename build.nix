let
  sources = import ./nix/sources.nix;
  tooling = import sources.nix-tooling;
  pkgs = tooling.pkgs;
  gis = import sources.gitignore { inherit (pkgs) lib; };
in
  pkgs.haskell.packages.ghc883.callCabal2nix "learn-brick" (gis.gitignoreSource ./.) {}
