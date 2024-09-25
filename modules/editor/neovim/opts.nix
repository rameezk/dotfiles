{ config, ... }:
let
  numSpaces = 4;
in
{
  background = "dark";
  termguicolors = true;

  relativenumber = true;
  number = true;
  cursorline = true;

  tabstop = numSpaces;
  softtabstop = numSpaces;
  shiftwidth = numSpaces;
  expandtab = true;

  smartindent = true;

  wrap = false;

  backspace = "indent,eol,start";

  ignorecase = true;
  smartcase = true;
  incsearch = true;
  hlsearch = false;

  splitright = true;
  splitbelow = true;

  swapfile = false;
  backup = false;
  undodir = "${config.xdg.configHome}/vim/undodir";
  undofile = true;

  scrolloff = 8;
  signcolumn = "yes";

  colorcolumn = "80";

  updatetime = 50;
}
