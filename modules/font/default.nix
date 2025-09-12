{
  pkgs,
  lib,
  config,
  ...
}:
{
  options = {
    fonts.enable = lib.mkEnableOption "Enable fonts";
  };

  config = lib.mkIf config.fonts.enable {
    home.packages = with pkgs; [
      meslo-lgs-nf
      hackgen-nf-font
      jetbrains-mono
      pkgs.nerd-fonts.jetbrains-mono
      pkgs.nerd-fonts.hack
    ];
  };
}
