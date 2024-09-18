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
  {
    mode = "n";
    key = "<leader>h";
    action = "<cmd>noh<cr>";
  }
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
]
