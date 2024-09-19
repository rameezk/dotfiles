{ lib, ... }:
{
  imports = [
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
