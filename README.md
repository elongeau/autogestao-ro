# autogestao

A CLI to manage tasks

## Install

### Pre-requisites

This project is based on Nix and uses nix flakes for building, so

1. [Install Nix](https://nixos.org/download.html)
2. [Enable flakes](https://nixos.wiki/wiki/Flakes#Enable_flakes)

Once done, you just have to 

```shell
nix build 'git+ssh://git@github.com/elongeau/autogestao-haskell-ro'
```

a `result` symbolic link will then be created in the current directory, you just have to run `result/bin/autogestao`

### Develop

This project is based on [Srid's Haskell Template](https://srid.ca/haskell-template).
