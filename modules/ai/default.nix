{
  pkgs,
  lib,
  config,
  ...
}:
{
  imports = [
    ./agents
    ./claude
    ./claude-skills
    ./pi
  ];

  options = {
    ai.enable = lib.mkEnableOption "enable ai";
  };

  config = lib.mkIf config.ai.enable {
    home.packages = with pkgs; [
      claude-code
    ];
  };
}
