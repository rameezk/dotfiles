{ pkgs, lib, config, ... }: {
    options = {
        cloud.azure.enable = lib.mkEnableOption "enable azure";
    };

    config = lib.mkIf config.cloud.azure.enable {
        home.packages = with pkgs; [ azure-cli ]; 
    };
}
