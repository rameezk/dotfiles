{
  pkgs,
  lib,
  config,
  ...
}:
{
  options = {
    core.enable = lib.mkEnableOption "enable core";
  };

  config = lib.mkIf config.core.enable {
    home.packages = with pkgs; [
      nixfmt-rfc-style # nix formatter
    ];
  };
}
