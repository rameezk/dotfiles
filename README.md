# dotfiles

Nix configuration using Home Manager and Nix Flakes.

## Prerequisites

1. [Nix](https://nixos.org/) with flakes enabled

## Installation

1. Clone this repo:
   ```sh
   git clone git@github.com:rameezk/dotfiles.git ~/.config/dotfiles
   cd ~/.config/dotfiles
   ```

2. Build and activate the configuration:
   ```sh
   ./bin/switch
   ```
   The machine configuration is automatically determined by hostname.

## Commands

| Command | Description |
|---------|-------------|
| `./bin/switch` | Build and activate configuration |
| `./bin/upgrade` | Update flake inputs and rebuild |
| `./bin/rollback` | Rollback to previous generation |
| `./bin/collect-garbage` | Clean up old generations |

## Development

### Formatting
Format Nix files:
```sh
nix fmt
```

### Pre-commit Hooks
This repo uses [pre-commit](https://pre-commit.com/) to format `*.nix` files with nixfmt.

To install hooks:
```sh
pre-commit install
```

## Other Docs
See the [docs](./docs) directory for additional documentation.

## Emacs
Interested in my Emacs config? See [modules/editor/emacs/config/emacs.org](modules/editor/emacs/config/emacs.org).

## Legacy
Looking for the old non-Nix dotfiles? Find them on the [old-school-thinking](https://github.com/rameezk/dotfiles/tree/old-school-thinking) branch.
