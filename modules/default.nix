{ lib, ... }:
{
  imports = [
    ./ai
    ./browser
    ./cloud
    ./container
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
