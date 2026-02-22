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

    verify.checks = [
      {
        type = "command";
        name = "node";
        desc = "Node.js";
      }
      {
        type = "command";
        name = "npm";
        desc = "NPM";
      }
    ];

    home.packages = with pkgs; [ nodejs ];
  };
}
