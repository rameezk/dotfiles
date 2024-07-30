{ config, pkgs, lib, ... }:

{
  # Opt-in to modules by including theme here.
  #  Is there a better way to do this per machine?
  imports = [
    # shell
    ../../modules/shell

    # editors
    ../../modules/editors/vim
    ../../modules/editors/jetbrains

    # vcs
    ../../modules/vcs/git

    # lang
    ../../modules/language/python
    ../../modules/language/nodejs
    ../../modules/language/java

    # tool
    ../../modules/tool/aws
    ../../modules/tool/tmux
    ../../modules/tool/secret-management
    ../../modules/tool/package-management

    # networking
    ../../modules/networking/proxy

    # MacOS
    ../../modules/os/macos/window-management
  ];

  proxy = {
    enable = true;
    protocol = "http";
    host = "localhost";
    port = 3128;
  };

  java = {
    enable = true;
    manageJDK = false;
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Packages
  home.packages = with pkgs; [ nixVersions.latest nixfmt-classic ];

  nix = {
    package = pkgs.nixVersions.latest;
    settings.experimental-features = [ "nix-command" "flakes" ];
    extraOptions = ''
      keep-outputs = true
      keep-derivations = true
    '';
  };

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
