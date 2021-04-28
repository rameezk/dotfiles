{ config, pkgs, ... }:

let
  username = "rameezk";
  homeDirectory = "/home/rameezk";
in
{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = username;
  home.homeDirectory = homeDirectory;

  # shell
  programs.fish = {
    enable = true;

    shellInit = ''
      # Disable fish greeting message
      set fish_greeting

      # Enable fenv for sourcing foreign environment variables. 
      # This is needed for sourcing the Nix path below.
      set -p fish_function_path ${pkgs.fishPlugins.foreign-env}/share/fish/vendor_functions.d

      # nix
      if test -e ${homeDirectory}/.nix-profile/etc/profile.d/nix.sh
        fenv source ${homeDirectory}/.nix-profile/etc/profile.d/nix.sh
      end
    '';
  };

  # vim
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

  # Packages
  home.packages = with pkgs; [
    # cli tools
    ripgrep # faster grepping
    tmux # terminal multiplexor
    jq # parsing JSON
    entr # file watching
    fzf # fuzzy file finder

    # git
    git
    git-crypt # encrypting git repos transparently

    # cloud
    azure-cli

    # kubernetes
    kubectl
    kubectx
    stern
  ];

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.05";
}
