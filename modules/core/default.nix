{
  pkgs,
  lib,
  config,
  ...
}:
{
  imports = [
    ./verify.nix
  ];

  options = {
    core.enable = lib.mkEnableOption "enable core";
  };

  config = lib.mkIf config.core.enable { };
}
