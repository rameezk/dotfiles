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
    ./vcs
  ];

  core.enable = lib.mkDefault true;
}
