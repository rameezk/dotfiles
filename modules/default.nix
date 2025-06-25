{ lib, ... }:
{
  imports = [
    ./ai
    ./browser
    ./cloud
    ./core
    ./editor
    ./font
    ./language
    ./network
    ./os
    ./shell
    ./theme
    ./vcs
  ];

  core.enable = lib.mkDefault true;
}
