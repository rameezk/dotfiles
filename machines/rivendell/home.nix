{ inputs, pkgs, ... }:

{
    imports = [
        ../../modules
    ];

    shell.enable = true;

    editor = {
        neovim.enable = true;
        jetbrains-vim-mode.enable = true;
    };

    vcs.git.enable = true;

    language = {
        python.enable = true;
        java = {
            enable = true;
            manageJDK = false;
        };
    };

    cloud.aws.enable = true;

    # Let Home Manager install and manage itself.
    programs.home-manager.enable = true;

    # Packages
    home.packages = with pkgs; [ nixVersions.latest nixfmt-classic asdf-vm ];

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
