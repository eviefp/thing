let
  sources = import ./nix;
  self = sources.tooling.haskell.ghc884;
in
  self.shell
