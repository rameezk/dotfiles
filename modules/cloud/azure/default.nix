{
  pkgs,
  lib,
  config,
  ...
}:
{
  options = {
    cloud.azure.enable = lib.mkEnableOption "enable azure";
  };

  config = lib.mkIf config.cloud.azure.enable {

    verify.checks = [
      {
        type = "command";
        name = "az";
        desc = "Azure CLI";
      }
    ];

    home.packages = with pkgs; [
      azure-cli
    ];
  };
}
