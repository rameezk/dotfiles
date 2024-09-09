{ pkgs, lib, config, ... }: {

    options = {
        editor.emacs.enable = lib.mkEnableOption "enable emacs";
    };

    config = lib.mkIf config.editor.emacs.enable {
        programs.emacs = {
            enable = true;
            package = pkgs.emacs-pgtk;
        };

        # Write init.el to the Nix store and symlink it to ~/.emacs.d/init.el
        #  This seems counter-intuitive, but I've had issues with nur/emacs-init previously
        home.file.".emacs.d/init.el".text = builtins.readFile ./config/init.el;

        home.packages = with pkgs; [
            # needed to compile sqlite for org-roam
            # for MacOS for some reason you also need to run `xcode-select --install` for some reason
            gcc

            # spell checking
            aspell
            aspellDicts.en # includes en_GB variant
            aspellDicts.en-computers
            aspellDicts.en-science

            # markdown
            multimarkdown

            # gpg
            pinentry-emacs
        ];
    };
}
