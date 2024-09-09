{ pkgs, lib, config, ... }: { 
    options = {
        cloud.aws.enable = lib.mkEnableOption "enable aws";
    };

    config = lib.mkIf config.cloud.aws.enable {
        home.packages = with pkgs; [ awscli2 ]; 
    };
}
