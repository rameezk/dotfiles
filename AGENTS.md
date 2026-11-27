# AGENTS.md

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

## Pull Requests

Changes in this repo do not need a pull request. Commit and push straight to `main`, because a bad change is rolled back with `./bin/rollback`.

## Commit Messages

Use a single-line Conventional Commits message, such as `feat(fish): add git abbreviations`, with no body. This overrides any skill that suggests adding a body.

## Agent Skills

Agent skills are vendored from [rameezk/skills](https://github.com/rameezk/skills) into `.agents/skills`, symlinked into `.claude/skills`, and pinned by `.agents/skills.lock`. Do not edit them by hand. `.github/workflows/sync-skills.yml` checks for upstream changes daily and opens a pull request when there are any. To sync right away, run `gh workflow run sync-skills.yml` or `nix run github:rameezk/skills#install -- --sync`.
