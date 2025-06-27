# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is a NixOS/Nix Darwin configuration repository using Home Manager and Nix Flakes. It manages dotfiles and system configurations for multiple machines (fangorn, rivendell, gondor, rohan) across macOS and Linux platforms.

## Essential Commands

### Building and Rebuilding Configuration
```bash
# Primary rebuild command (builds and activates configuration)
./bin/dot rebuild

# Rebuild with trace output for debugging
./bin/dot rebuild --with-traces

# Update flake inputs and rebuild
nix flake update && ./bin/dot rebuild
```

### Development Environment
```bash
# Enter development shell with necessary tools
nix-shell

# Format Nix files
nix fmt

# Edit configuration files interactively
./bin/dot edit

# Edit with vim instead of emacs
./bin/dot edit --vim
```

### Machine Management
```bash
# Set target machine (required before rebuild)
echo "fangorn" > .machine   # or rivendell, gondor, rohan

# Rollback to previous generation
./bin/dot rollback
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
2. Test changes with `./bin/dot rebuild`
3. Use `nix fmt` to format Nix files before committing
4. Pre-commit hooks automatically format .nix files (nixfmt) and ./bin/dot (black)

## Machine Targeting

The build system uses a `.machine` file to determine which configuration to build. Each machine configuration inherits from shared modules but can override settings as needed.

## Secret Management

Secrets are managed through SOPS and stored in a separate private repository. The `mysecrets` flake input provides encrypted configuration files that are decrypted during activation.