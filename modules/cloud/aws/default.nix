{
  pkgs,
  lib,
  config,
  ...
}:
{
  options = {
    cloud.aws.enable = lib.mkEnableOption "enable aws";
  };

  config = lib.mkIf config.cloud.aws.enable {

    verify.checks = [
      {
        type = "command";
        name = "aws";
        desc = "AWS CLI";
      }
    ];

    home.packages = with pkgs; [ awscli2 ];
  };
}
