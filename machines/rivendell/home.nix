{ inputs, pkgs, ... }:

{
    # Opt-in to modules by including theme here.
    #  Is there a better way to do this per machine?
    imports = [
        # shell
        ../../modules/shell

        # editors
        ../../modules/editors/neovim
        ../../modules/editors/jetbrains

        # vcs
        ../../modules/vcs/git

        # lang
        ../../modules/language/python
        ../../modules/language/java

        # tool
        ../../modules/tool/aws
        ../../modules/tool/tmux
        ../../modules/tool/package-management
    ];

    java = {
        enable = true;
        manageJDK = false;
    };

    # Let Home Manager install and manage itself.
    programs.home-manager.enable = true;

    # Packages
    home.packages = with pkgs; [ nixVersions.latest nixfmt-classic ];

    # nix = {
    #   package = pkgs.nixVersions.latest;
    #   settings.experimental-features = [ "nix-command" "flakes" ];
    #   extraOptions = ''
    #     keep-outputs = true
    #     keep-derivations = true
    #   '';
    # };

    # This value determines the Home Manager release that your
    # configuration is compatible with. This helps avoid breakage
    # when a new Home Manager release introduces backwards
    # incompatible changes.
    #
    # You can update Home Manager without changing this value. See
    # the Home Manager release notes for a list of state version
    # changes in each release.
    home.stateVersion = "20.09";

    sops = {
        # defaultSopsFile = ../../secrets/secrets.yaml;
        defaultSopsFile = "${builtins.toString inputs.mysecrets}/secrets.yaml";
        validateSopsFiles = false;

        age.keyFile = "/Users/rameezk/.config/sops/age/keys.txt";

        secrets = {
            "private_keys/ssh/rameezk" = {
                path = "/Users/rameezk/.ssh/id_ed25519";
                mode = "0600";
            };
            "private_keys/gpg/rameezk" = {
                path = "/Users/rameezk/.config/gpg/rameezk_private.gpg";
            };
        };
    };
}
