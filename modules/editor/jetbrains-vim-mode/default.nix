{ lib, config, ... }: {
    options = {
        editor.jetbrains-vim-mode.enable = lib.mkEnableOption "enable jetbrains vim mode";
    };

    config = lib.mkIf config.editor.jetbrains-vim-mode.enable {
        home.file.".ideavimrc".text = #vim 
            ''
            syntax on "Turn on syntax highlighting

            " plugins
            set surround
            set commentary

            set iskeyword+=_

            set clipboard+=unnamedplus "use clipboard

            set incsearch "Incremental search
            set ignorecase "Ignore case
            set smartcase "Smart case matching

            set relativenumber "turn on relative line numbers
            set number "turn on current line number

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
    };
}
