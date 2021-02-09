{ config, lib, pkgs, ... }:

let
  username = "rameezk";
  homeDirectory = "/Users/rameezk";
in
{
  # Let Home Manager install and manage itself.
#   programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
#   home.username = username;
#   home.homeDirectory = homeDirectory;

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
#   home.stateVersion = "20.09";

  # General packages
#   home.packages = with pkgs; [
#     tmux
#   ];

  programs.fish.enable = true;



  system.stateVersion = 4;
  environment.systemPath = [ /run/current-system/sw/bin ];
 

   environment.systemPackages =
    [ pkgs.tmux
      pkgs.fish
      pkgs.vim ];

nix = {
    package = pkgs.nixFlakes;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
   };
}
