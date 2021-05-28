{ pkgs, ... }: {

  programs.vim = {
    enable = true;
    plugins = with pkgs.vimPlugins; [
      vim-sensible
      vim-airline
      vim-commentary
      gruvbox
    ];
    extraConfig = ''
      set background=dark " Better colours for darker backgrounds
      set number "Turn on line numbers
      set cursorline " Highlight the current line the cursor is on
      set visualbell " Turn on visual bell
      colorscheme gruvbox " Turn on gruvbox colorscheme
    '';
  };

}
