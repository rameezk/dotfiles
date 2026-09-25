{
  lib,
  config,
  pkgs,
  ...
}:

{
  options = {
    editor.neovim.enable = lib.mkEnableOption "enable neovim";
  };

  config = lib.mkIf config.editor.neovim.enable {

    verify.checks = [
      {
        type = "command";
        name = "nvim";
        desc = "Neovim editor";
      }
    ];

    programs.nixvim = {
      enable = true;

      nixpkgs.config = pkgs.config;
      nixpkgs.source = pkgs.path;

      viAlias = true;
      vimAlias = true;
      vimdiffAlias = true;

      plugins = {
        lualine.enable = true;
        transparent.enable = true;
        gitsigns.enable = true;
        web-devicons.enable = true;
        telescope.enable = true;

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

        harpoon = {
          enable = true;
          enableTelescope = true;
        };

      };

      globals = {
        mapleader = " ";
      };

      opts = import ./opts.nix { inherit config; };

      keymaps = import ./keymaps.nix { };

      autoCmd = [
        {
          event = [ "FileType" ];
          pattern = [
            "markdown"
            "text"
            "gitcommit"
          ];
          callback = {
            __raw = ''
              function()
                vim.opt_local.wrap = true
                vim.opt_local.linebreak = true
                vim.opt_local.breakindent = true
                vim.opt_local.colorcolumn = ""
                local opts = { buffer = true, silent = true }
                vim.keymap.set({ "n", "x" }, "j", "v:count == 0 ? 'gj' : 'j'", vim.tbl_extend("force", opts, { expr = true }))
                vim.keymap.set({ "n", "x" }, "k", "v:count == 0 ? 'gk' : 'k'", vim.tbl_extend("force", opts, { expr = true }))
                vim.keymap.set({ "n", "x" }, "0", "g0", opts)
                vim.keymap.set({ "n", "x" }, "$", "g$", opts)
                vim.keymap.set({ "n", "x" }, "<Down>", "v:count == 0 ? 'gj' : 'j'", vim.tbl_extend("force", opts, { expr = true }))
                vim.keymap.set({ "n", "x" }, "<Up>", "v:count == 0 ? 'gk' : 'k'", vim.tbl_extend("force", opts, { expr = true }))
                vim.keymap.set("i", "<Down>", "<C-o>gj", opts)
                vim.keymap.set("i", "<Up>", "<C-o>gk", opts)
              end
            '';
          };
        }
      ];

    };
  };
}
