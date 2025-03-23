{ ... }:
[
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

  # Kill highlights
  {
    mode = "n";
    key = "<leader>h";
    action = "<cmd>noh<cr>";
  }

  # Yank to system clipboard
  {
    mode = "n";
    key = "<leader>Y";
    action = ''"*Y'';
  }
  {
    mode = [
      "n"
      "v"
    ];
    key = "<leader>y";
    action = ''"*y'';
  }

  # Keep what was originally copied
  {
    mode = "x";
    key = "<leader>p";
    action = ''"_dP'';
  }

  # move selected lines up/down
  {
    mode = "v";
    key = "J";
    action = ":m '>+1<CR>gv=gv";
  }
  {
    mode = "v";
    key = "K";
    action = ":m '<-2<CR>gv=gv";
  }
]
