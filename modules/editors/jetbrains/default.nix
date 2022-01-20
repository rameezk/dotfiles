{ pkgs, ... }:

{
  home.file.".ideavimrc".text = ''
    syntax on "Turn on syntax highlighting

    " plugins
    set surround
    set commentary

    set iskeyword+=_

    set clipboard+=unnamedplus "use clipboard

    set incsearch "Incremental search
    set ignorecase "Ignore case
    set smartcase "Smart case matching

    let mapleader=" "

    " always make these typos
    :command WQ wq
    :command Wq wq
    :command W w
    :command Q q
    :command Wall wall
    :command WAll wall

    " fancy bindings yo
    :nnoremap <leader>fs :w *<CR>
    :nnoremap <leader>fS :wall *<CR>
    :nmap <leader>cl gcc
    :nnoremap <leader>wv :vsplit <CR>
    :nnoremap <leader>ws :split <CR>
  '';
}
