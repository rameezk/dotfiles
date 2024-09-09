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

                cmp-path.enable = true;
                cmp-nvim-lsp.enable = true;
                cmp-buffer.enable = true;
                cmp = {
                    enable = true;
                    autoEnableSources = true;
                    settings = {
                        sources = [
                            { name = "nvim_lsp"; }
                            { name = "path"; }
                            { name = "buffer"; }
                        ];
                        mapping = {
                            "<C-Space>" = "cmp.mapping.complete()";
                            "<C-d>" = "cmp.mapping.scroll_docs(-4)";
                            "<C-e>" = "cmp.mapping.close()";
                            "<C-f>" = "cmp.mapping.scroll_docs(4)";
                            "<CR>" = "cmp.mapping.confirm({ select = true })";
                            "<S-Tab>" = "cmp.mapping(cmp.mapping.select_prev_item(), {'i', 's'})";
                            "<Tab>" = "cmp.mapping(cmp.mapping.select_next_item(), {'i', 's'})";
                        };
                    };
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
                    key = "<leader>fe";
                    action = "<cmd>Explore<cr>";
                }
                {
                    mode = "n";
                    key = "<leader>ff";
                    action = "<cmd>Telescope find_files<cr>";
                }
                {
                    mode = "n";
                    key = "<leader>fs";
                    action = "<cmd>Telescope live_grep<cr>";
                }
            ];
        };
    };
}
