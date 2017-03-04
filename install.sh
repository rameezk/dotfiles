#!/usr/bin/env bash

# Get current working directory so we can run the file from anywhere

export DOTFILES_DIR
DOTFILES_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Symlinking the files
ln -sfv "$DOTFILES_DIR/brew/Brewfile" ~

# Setup package managers

. "$DOTFILES_DIR/brew/brew-install.sh"
