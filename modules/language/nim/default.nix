{
  pkgs,
  lib,
  config,
  ...
}:
{
  options = {
    language.nim.enable = lib.mkEnableOption "enable nim";
  };

  config = lib.mkIf config.language.nim.enable {
    home.packages = with pkgs; [
      nim
      nimlsp
    ];
  };
}
