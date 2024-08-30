{ pkgs, ... }: {
  programs.neovim = {
    enable = true;

    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;

    plugins = with pkgs.vimPlugins; [
      {
        plugin = tokyonight-nvim;
        type = "lua";
        config = # lua
          ''
                local fg_gutter = "#627E97"

                require("tokyonight").setup({
                        style = "night",
                        on_colors = function(colors)
                        colors.fg_gutter = fg_gutter
                        end,
                        })

            vim.cmd([[colorscheme tokyonight]])
          '';
      }
      {
        plugin = nvim-treesitter.withAllGrammars;
        type = "lua";
        config = ''
                  local parser_install_dir = vim.fn.stdpath("cache") .. "/treesitters"
                  vim.fn.mkdir(parser_install_dir, "p")
                  vim.opt.runtimepath:append(parser_install_dir)
          	require'nvim-treesitter.configs'.setup {
                  parser_install_dir = parser_install_dir,
                  ensure_installed = {},
          		-- Install parsers synchronously (only applied to `ensure_installed`)
          		sync_install = false,

          		-- Automatically install missing parsers when entering buffer
          		-- Recommendation: set to false if you don't have `tree-sitter` CLI installed locally
          		auto_install = true,

          		highlight = {
          		  enable = true,
          		  additional_vim_regex_highlighting = false,
          		},
          	}     
        '';
      }
    ];

    extraLuaConfig = ''
      vim.opt.background = "dark"
      vim.opt.termguicolors = true

      vim.opt.relativenumber = true
      vim.opt.number = true;
      vim.opt.cursorline = true -- highlight current line

      vim.opt.tabstop = 4
      vim.opt.softtabstop = 4
      vim.opt.shiftwidth = 4
      vim.opt.expandtab = true

      vim.opt.wrap = false

      vim.opt.backspace = "indent,eol,start" -- allow backspace

      vim.g.mapleader = " " -- set leader to SPC
      vim.keymap.set("n", "<leader>ws", ":split ", { desc = "Split window horizontally and choose file to edit" })
      vim.keymap.set("n", "<leader>wv", ":vsplit ", { desc = "Split window vertically and choose file to edit" })
    '';
  };
}

