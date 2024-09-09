{ lib, config, ... }: {
    options = {
        language.latex.enable = lib.mkEnableOption "enable latex";
    };

    config = lib.mkIf config.language.latex.enable {
        programs.texlive = {
            enable = true;
            extraPackages = tpkgs: { inherit (tpkgs) scheme-full; };
        };
    };
}
