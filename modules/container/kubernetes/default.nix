{
  pkgs,
  lib,
  config,
  ...
}:
{
  options = {
    container.kubernetes.enable = lib.mkEnableOption "enable kubernetes tools";
  };

  config = lib.mkIf config.container.kubernetes.enable {
    home.packages = with pkgs; [
      kubectl
      kubectx
    ];
  };
}
