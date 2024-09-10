{
  pkgs,
  lib,
  config,
  ...
}:
{
  options = {
    language.rust.enable = lib.mkEnableOption "enable rust";
  };

  config = lib.mkIf config.language.rust.enable {
    home.packages = with pkgs; [
      rustc
      cargo
      rustfmt
    ];
  };
}
