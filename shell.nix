let
  tooling = import ./default.nix;
  self = tooling.haskell.ghc884;
in
  self.shell
