#!/usr/bin/env bash

/usr/bin/ruby -e "$(curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim)"

echo "VIM install script complete!"
