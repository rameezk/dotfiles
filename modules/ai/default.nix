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
    ./omp.nix
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
