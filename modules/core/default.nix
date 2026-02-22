{
  pkgs,
  lib,
  config,
  ...
}:
{
  options = {
    core.enable = lib.mkEnableOption "enable core";
  };

  config = lib.mkIf config.core.enable { };
}
