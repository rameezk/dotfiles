{
  pkgs,
  lib,
  config,
  ...
}:
{
  options = {
    language.python.enable = lib.mkEnableOption "enable python";
  };

  config = lib.mkIf config.language.python.enable {
    home.packages = with pkgs; [
      (python312.withPackages (
        ps: with ps; [
          pipx
        ]
      ))
      stdenv.cc.cc.lib
    ];
  };
}
