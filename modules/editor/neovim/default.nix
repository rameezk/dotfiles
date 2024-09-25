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

      colorschemes.catppuccin = {
        enable = true;
        settings = {
          transparent_background = true;
          flavour = "frappe";
          integrations = {
            cmp = true;
            gitsigns = true;
            treesitter = true;
          };
        };
      };

      plugins = {
        lualine.enable = true;
        transparent.enable = true;
        gitsigns.enable = true;
        web-devicons.enable = true;

        lsp = import ./lsp.nix { };

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
            highlight = {
              enable = true;
            };
            indent.enable = true;
            sync_install = false;
          };
        };

        telescope.enable = true;

      };

      globals = {
        mapleader = " ";
      };

      opts = import ./opts.nix { };

      keymaps = import ./keymaps.nix { };

    };
  };
}
