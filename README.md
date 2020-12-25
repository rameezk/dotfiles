# dotfiles

> A saner, more predictable way of managing my machines.

## Quickstart
At the moment, this is targetting only MacOS. 
In future, I hope to port this over to my other Linux machines as well.

1. Install Nix
```bash
sh <(curl -L https://nixos.org/nix/install) --darwin-use-unencrypted-nix-store-volume
```
2. Temporarily source the neccessary environment variables
```bash
. ~/.nix-profile/etc/profile.d/nix.sh
```
4. Install nix-darwin
```bash
nix-build https://github.com/LnL7/nix-darwin/archive/master.tar.gz -A installer
./result/bin/darwin-installer
```
3. Install nix flakes
```bash
nix-env -iA nixpkgs.nixFlakes
```
4. Clone repo
```bash
git clone git@github.com:rameezk/dotfiles.git ~/.dotfiles
```
5. Build dotfiles
```bash
~/.dotfiles/bin/dot build
```