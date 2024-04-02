{ pkgs, ... }: {

  programs.vim = {
    enable = true;
    plugins = with pkgs.vimPlugins; [
      vim-sensible
      vim-airline
      vim-commentary
      gruvbox
      nim-vim
      vim-terraform
      vim-signify # show modified lines for VSC in the gutter
    ];
    extraConfig = ''
      set background=dark " Better colours for darker backgrounds
      set relativenumber "turn on relative line numbers
      set number "turn on current line number
      set cursorline " Highlight the current line the cursor is on
      set visualbell " Turn on visual bell
      colorscheme gruvbox " Turn on gruvbox colorscheme
      set backspace=indent,eol,start " More powerfull backspacing
      au BufEnter * set fo-=c fo-=r fo-=o " Turn off inserting a comment on the next line if the current line has one
    '';
  };

}
