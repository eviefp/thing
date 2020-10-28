# thing

- console application
- create nix-aware new projects using templates and nix-tooling
- manage projects (at least niv source?)

## TODO
- [ ] Create new project
    - [x] Basic project template
    - [x] Post-create hooks:
              be able to run commands after template creation
              potentially via `nix-shell --command ...`
    - [x] Grab templates from github in addition to local paths
    - [ ] Extra attributes read from env or `~/.thing.yaml`
              e.g. github URL, name, etc. (fill in in `package.yaml`)



## Much Much Later TODO
- add a TUI later after the main code is done
