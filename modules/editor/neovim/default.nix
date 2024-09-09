{ lib, config, ... }:

{
    options = {
        editor.neovim.enable = lib.mkEnableOption "enable neovim";
    };

    config = lib.mkIf config.editor.neovim.enable {
        programs.nixvim = {
            enable = true;

            viAlias = true;
            vimAlias = true;
            vimdiffAlias = true;

            colorschemes.tokyonight = {
                enable = true;
                settings = {
                    on_colors = # lua
                        ''
            function(colors)
                colors.fg_gutter = "#627E97"
            end
            '';
                    transparent = true;
                };
            };

            plugins = {
                lualine.enable = true;
                transparent.enable = true;

                lsp = {
                    enable = true;
                    servers = {
                        nixd.enable = true;
                        lua-ls.enable = true;
                        java-language-server.enable = true;
                    };
                };

                cmp = {
                    enable = true;
                    autoEnableSources = true;
                };

                treesitter = {
                    enable = true;
                    settings = {
                        auto_install = true;
                        ensure_installed = "all";
                        highlight = { enable = true; };
                        indent.enable = true;
                        sync_install = false;
                    };
                };

                telescope.enable = true;

            };

            globals = {
                mapleader = " ";
            };

            opts = {
                background = "dark";
                termguicolors = true;

                relativenumber = true;
                number = true;
                cursorline = true;

                tabstop = 4;
                softtabstop = 4;
                shiftwidth = 4;
                expandtab = true;

                wrap = false;

                backspace = "indent,eol,start";

                ignorecase = true;
                smartcase = true;
                incsearch = true;
            };

            keymaps = [
                {
                    mode = "n";
                    key = "<leader>ff";
                    action = "<cmd>Telescope find_files<cr>";
                }
            ];
        };
    };
}
