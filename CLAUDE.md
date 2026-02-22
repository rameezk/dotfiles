# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is a NixOS/Nix Darwin configuration repository using Home Manager and Nix Flakes. It manages dotfiles and system configurations for multiple machines (fangorn, rivendell) across macOS and Linux platforms.

## Essential Commands

### Building and Rebuilding Configuration
```bash
# Primary rebuild command (builds and activates configuration)
./bin/switch

# Update flake inputs and rebuild
./bin/upgrade

# Rollback to previous generation
./bin/rollback

# Clean up old generations
./bin/collect-garbage
```

### Development Environment
```bash
# Enter development shell with necessary tools
nix-shell

# Format Nix files
nix fmt
```

## Architecture

### Flake Structure
- **flake.nix**: Main flake configuration with inputs and outputs
- **machines/**: Machine-specific configurations
  - Each machine has `default.nix` (system config) and `home.nix` (home-manager config)
- **modules/**: Reusable configuration modules organized by category
- **secrets/**: SOPS-encrypted secrets using nix-secrets repo

### Module Categories
- **core/**: Essential system utilities and base configuration
- **shell/**: Terminal environments (fish, tmux, starship, wezterm)
- **editor/**: Text editors (neovim with nixvim, emacs, vim)
- **language/**: Programming language support (python, rust, nodejs, java, etc.)
- **os/**: OS-specific configurations (macOS window management, Linux desktop)
- **vcs/**: Version control (git configuration)
- **cloud/**: Cloud provider tools (AWS, Azure)
- **ai/**: AI development tools
- **theme/**: Theming (catppuccin)

### Key Technologies
- **Home Manager**: User environment management
- **Nix Darwin**: macOS system configuration
- **nixvim**: Neovim configuration in Nix
- **SOPS**: Secrets management
- **Homebrew**: macOS package management via nix-homebrew

## Development Workflow

1. Modify configuration files in appropriate modules
2. Test changes with `./bin/switch`
3. Use `nix fmt` to format Nix files before committing
4. Pre-commit hooks automatically format .nix files (nixfmt)

## Machine Targeting

The build system uses nix-darwin with flakes. The machine configuration is determined by hostname automatically when running `darwin-rebuild switch --flake .`. Each machine configuration inherits from shared modules but can override settings as needed.

## Secret Management

Secrets are managed through SOPS and stored in a separate private repository. The `mysecrets` flake input provides encrypted configuration files that are decrypted during activation.