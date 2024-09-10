{
  pkgs,
  lib,
  config,
  ...
}:
{
  options = {
    language.nodejs.enable = lib.mkEnableOption "enable nodejs";
  };

  config = lib.mkIf config.language.nodejs.enable {
    home.packages = with pkgs; [ nodejs ];
  };
}
