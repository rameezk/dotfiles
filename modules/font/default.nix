{
  pkgs,
  lib,
  config,
  ...
}:
{
  options = {
    fonts.enable = lib.mkEnableOption "Enable fonts";
  };

  config = lib.mkIf config.fonts.enable {
    home.packages = with pkgs; [ fira-code ];
  };
}
