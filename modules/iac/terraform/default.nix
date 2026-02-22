{ lib, ... }:
{
  options = {
    iac.terraform.enable = lib.mkEnableOption "enable terraform";
  };
}
