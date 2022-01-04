{ config, pkgs, lib, ... }:

{
  # Opt-in to modules by including theme here.
  #  Is there a better way to do this per machine?
  imports = [
    # shell
    ../../modules/shell

    # editors
    ../../modules/editors/emacs
    ../../modules/editors/vim

    # vcs
    ../../modules/vcs/git

    # lang
    ../../modules/language/python
    ../../modules/language/clojure
    ../../modules/language/nim
    ../../modules/language/nodejs
    ../../modules/language/java

    # tools
    ../../modules/tool/azure
    ../../modules/tool/aws
    ../../modules/tool/kubernetes
    ../../modules/tool/helm
    ../../modules/tool/media
  ];

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Packages
  home.packages = with pkgs; [ nixUnstable nixfmt ];

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
