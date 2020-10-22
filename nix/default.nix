let
  sources = import ./sources.nix;
  tooling = import sources.nix-tooling;
  gis = import sources.gitignore { inherit (tooling.pkgs) lib; };
in
  {
    tooling = tooling;
    gis = gis;
  }

