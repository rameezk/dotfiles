# dotfiles

## Where are the old .files?
Through my progression and learning of the [Nix](https://nixos.org/) ecosystem, I discovered [Home Manager](https://github.com/nix-community/home-manager).

As such I've declared my old .files as __old-school-thinking__. But you can still find them all [here](https://github.com/rameezk/dotfiles/tree/old-school-thinking).

## Prerequisites
1. [Nix](https://nixos.org/)
2. [Home Manager](https://github.com/nix-community/home-manager)

## Installation
Clone this repo to `~/.config/dotfiles`
```sh
git clone git@github.com:rameezk/dotfiles.git ~/.config/dotfiles
```

Symlink `home.nix` (not sure if symlinking makes sense yet).
```sh
ln -sfv ./home.nix ~/.config/nixpkgs/home.nix
```
