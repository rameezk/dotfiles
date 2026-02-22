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
    ./iac
    ./language
    ./network
    ./os
    ./shell
    ./theme
    ./vcs
  ];

  core.enable = lib.mkDefault true;
}
