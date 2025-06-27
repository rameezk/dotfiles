{
  pkgs,
  lib,
  config,
  ...
}:
{
  options = {
    container.enable = lib.mkEnableOption "enable container runtime";
  };

  config = lib.mkIf config.container.enable {
    home.packages = with pkgs; [
      podman
      podman-compose
    ];
  };
}
