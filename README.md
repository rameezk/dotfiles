# dotfiles

## Where are the old .files?
Through my progression and learning of the [Nix](https://nixos.org/) ecosystem, I discovered [Home Manager](https://github.com/nix-community/home-manager).

As such I've declared my old .files as __old-school-thinking__. But you can still find them all [here](https://github.com/rameezk/dotfiles/tree/old-school-thinking).

## Prerequisites
1. [Nix](https://nixos.org/)
2. [Home Manager](https://github.com/nix-community/home-manager)

## Installation
1. Clone this repo to `~/.config/dotfiles`
   ```sh
   git clone git@github.com:rameezk/dotfiles.git ~/.config/dotfiles
   ```

2. cd to `~/.config/dotfiles` and execute the following
   ```sh
   nix-shell
   ```
   This will give you the bare minimum packages to execute the `dot` binary in the next step.

3. Specify machine in file `.machine`. You can find machines in the `./machines/` directory.
   ```sh
   echo "<machine>" > .machine
   ```

4. Execute the environment rebuild
   ```sh
   ./bin/dot rebuild
   ```
   
## Docs
For other docs see `docs/` directory.

## Nice to have
### pre-commit hooks
This repo is setup for git hooks using the the [pre-commit framework](https://pre-commit.com/). 

Currently, hooks exist to format:
- *.nix files using NixFmt
- ./bin/dot file using Python black

To install the hooks, run:

```sh
pre-commit install
```

## Emacs
If you're interested in perusing my Emacs config you can do so [here](modules/editors/emacs/config/emacs.org).
