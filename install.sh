#!/usr/bin/env bash

# Get current working directory so we can run the file from anywhere

export DOTFILES_DIR
DOTFILES_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Symlinking the files
ln -sfv "$DOTFILES_DIR/brew/Brewfile" ~
ln -sfv "$DOTFILES_DIR/zsh/.zshrc" ~
ln -sfv "$DOTFILES_DIR/vim/.vimrc" ~

# Run installers

. "$DOTFILES_DIR/brew/brew-install.sh"
. "$DOTFILES_DIR/zsh/zsh-install.sh"
. "$DOTFILES_DIR/vim/vim-install.sh"
