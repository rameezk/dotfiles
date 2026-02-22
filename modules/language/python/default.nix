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

    verify.checks = [
      {
        type = "command";
        name = "python3";
        desc = "Python 3";
      }
      {
        type = "command";
        name = "pipx";
        desc = "Pipx";
      }
    ];

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
