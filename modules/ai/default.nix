{
  pkgs,
  lib,
  config,
  ...
}:
{
  imports = [
    ./agents
    ./claude-skills
    ./pi.nix
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
