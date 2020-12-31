# dotfiles

> A saner, more predictable way of managing my machines.

## Quickstart
At the moment, this is targetting only MacOS. 
In future, I hope to port this over to my other Linux machines as well.

1. Clone repo and cd into it
```bash
git clone git@github.com:rameezk/dotfiles.git ~/.dotfiles && cd ~/.dotfiles
```
2. Install Nix
```bash
sh <(curl -L https://nixos.org/nix/install) --darwin-use-unencrypted-nix-store-volume
```
3. Install nix-darwin
```bash
nix-build https://github.com/LnL7/nix-darwin/archive/master.tar.gz -A installer
./result/bin/darwin-installer
```
4. Install nix flakes
```bash
nix-env -iA nixpkgs.nixFlakes
```
5. Clone repo
```bash
git clone git@github.com:rameezk/dotfiles.git ~/.dotfiles
```
6. Build dotfiles
```bash
~/.dotfiles/bin/dot build
```