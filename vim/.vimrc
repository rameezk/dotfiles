" Rameez Khan

" Plugins {{{

set nocompatible " just in case - better safe than sorry ;)

call plug#begin('~/.vim/plugged')

Plug 'tpope/vim-sensible'
Plug 'scrooloose/nerdtree'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'morhetz/gruvbox'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

call plug#end()

" }}}

" UI Layout {{{

" Colours {{{

colorscheme gruvbox
set background=dark

" }}}

" Editor {{{

syntax enable " enable syntax processing

set guifont=Source\ Code\ Pro:h9

set number "turn on line numbers
set visualbell "turn on visual bell
set showcmd " show command in the bottom bar

set cursorline

filetype plugin indent on "load filetype specific indent files
set tabstop=4 "set tab stop to 4
set softtabstop=4 "number of spaces in tab when editing
set shiftwidth=4 "when indenting with >
set expandtab "tabs to spaces
set backspace=indent,eol,start "backspace properly

set wildmenu "visual autocomplete for command menu

set lazyredraw "redraw only when we really need to

set showmatch "braces mathing

set incsearch "incremental search as chars are entered
set hlsearch "highlight matches

" }}}

" }}}

" Keybindings {{{

let mapleader = "," "remap leader key

map <Leader>n :NERDTreeToggle<CR>

nmap <silent> <leader>ev :e $MYVIMRC<CR>
nmap <silent> <leader>sv :so $MYVIMRC<CR>

" }}}

" Organisation {{{

set modelines=1 "check the final line of this .vimrc file for a modeline

" }}}

" vim:foldmethod=marker:foldlevel=0
