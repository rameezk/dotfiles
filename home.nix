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

    interactiveShellInit = ''
      # Disable fish greeting message
      set fish_greeting

      # vi mode
      fish_vi_key_bindings

      # Enable fenv for sourcing foreign environment variables. 
      # This is needed for sourcing the Nix path below.
      set -p fish_function_path ${pkgs.fishPlugins.foreign-env}/share/fish/vendor_functions.d

      # nix
      if test -e ${homeDirectory}/.nix-profile/etc/profile.d/nix.sh
        fenv source ${homeDirectory}/.nix-profile/etc/profile.d/nix.sh
      end

      # editor
      export EDITOR=vim

      # colorscheme
      theme_gruvbox dark medium
      set -U fish_color_command b8bb26 # fish's default command color is a horrible dark blue, make it a nicer green

      # direnv
      eval (direnv hook fish)

      # asdf
      source ~/.asdf/asdf.fish

      # pipx
      set PATH $PATH /home/rameezk/.local/bin

      # Make gpg-agent play nicely with tmux
      export GPG_TTY=(tty)
    '';

    shellAbbrs = {
      gcm = "git commit -m";
      hm-rm-old-generations = "home-manager generations | tail -n +2 | awk '{ print $5 }' | xargs home-manager remove-generations";
    };

    plugins = [
      {
        name = "fish-gruvbox";
        src = pkgs.fetchFromGitHub {
          owner = "Jomik";
          repo = "fish-gruvbox";
          rev = "d8c0463518fb95bed8818a1e7fe5da20cffe6fbd";
          sha256 = "0hkps4ddz99r7m52lwyzidbalrwvi7h2afpawh9yv6a226pjmck7";
        };
      }
    ];
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

  # bat
  programs.bat = {
    enable = true;
    config = {
      theme = "gruvbox-dark";
      pager = "less -FR";
    };
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
  home.stateVersion = "20.09";
}
