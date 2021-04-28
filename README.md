# dotfiles

## Where are the old .files?
Through my progression and learning of the [Nix](https://nixos.org/) ecosystem, I discovered [Home Manager](https://github.com/nix-community/home-manager).

As such I've declared my old .files as __old-school-thinking__. But you can still find them all [here](https://github.com/rameezk/dotfiles/tree/old-school-thinking).

## Installation

Symlink `home.nix` (not sure if symlinking makes sense yet).
```sh
ln -sfv ./home.nix ~/.config/nixpkgs/home.nix
```
