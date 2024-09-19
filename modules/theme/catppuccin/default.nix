{ lib, config, ... }:
let
  cfg = config.theme.catppuccin;
in
{
  options.theme.catppuccin = {
    enable = lib.mkEnableOption "enable catppuccin";

    flavour = lib.mkOption {
      type = lib.types.str;
      default = "frappe";
    };
  };

  config = lib.mkIf cfg.enable {
    catppuccin = {
      enable = true;
      flavor = cfg.flavour;
    };
  };
}
