{ config, pkgs, lib, ... }:

{
  # Opt-in to modules by including theme here.
  #  Is there a better way to do this per machine?
  imports = [
    # core (needed as a bare minimum)
    ../../modules/core

    # shell
    ../../modules/shell

    # editors
    ../../modules/editors/vim
    ../../modules/editors/emacs
    ../../modules/editors/jetbrains

    # vcs
    ../../modules/vcs/git

    # lang
    ../../modules/language/python
    ../../modules/language/nodejs
    ../../modules/language/clojure

    # tool
    ../../modules/tool/aws
    ../../modules/tool/kubernetes
    ../../modules/tool/helm
    ../../modules/tool/tmux
  ];

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Packages
  home.packages = with pkgs; [ nixUnstable nixfmt ];

  # Add experimental features to nix configuration
  home.file.nixConf.text = ''
    experimental-features = nix-command flakes
  '';

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