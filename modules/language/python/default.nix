{
  pkgs,
  lib,
  config,
  ...
}:
{
  options = {
    language.python.enable = lib.mkEnableOption "enable python";
    language.python.extraPackages = lib.mkOption {
      type = lib.types.listOf (lib.types.functionTo (lib.types.listOf lib.types.package));
      default = [ ];
      description = ''
        Extra Python packages to fold into the wrapped python.
        Each entry is a function `ps: [ ps.<pkg> ... ]` so packages resolve
        against the wrapper's package set. Other modules contribute by setting
        this option (lists merge automatically).
      '';
    };
  };

  config = lib.mkIf config.language.python.enable {

    verify.checks = [
      {
        type = "command";
        name = "python3";
        desc = "Python 3";
      }
    ];

    home.packages = with pkgs; [
      (python312.withPackages (ps: lib.flatten (map (f: f ps) config.language.python.extraPackages)))
      stdenv.cc.cc.lib
    ];
  };
}
