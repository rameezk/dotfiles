{ pkgs, ... }:

let
    proxyProtocol = "http";
    proxyHost = "localhost";
    proxyPort = 3128;
in {
    # Opt-in to modules by including theme here.
    #  Is there a better way to do this per machine?
    imports = [
        ../../modules

        # vcs
        ../../modules/vcs/git

        # tool
        ../../modules/tool/aws
        ../../modules/tool/tmux
        ../../modules/tool/secret-management
        ../../modules/tool/package-management

        # networking
        ../../modules/networking/proxy

        # MacOS
        ../../modules/os/macos/window-management
    ];

    proxy = {
        enable = true;
        protocol = proxyProtocol;
        host = proxyHost;
        port = proxyPort;
    };

    shell.enable = true;

    editor.neovim.enable = true;
    editor.jetbrains-vim-mode.enable = true;

    fonts.enable = true;

    language.python.enable = true;
    language.nodejs.enable = true;
    language.java = {
        enable = true;
        manageJDK = false;

        enableProxy = true;
        proxyProtocol = proxyProtocol;
        proxyHost = proxyHost;
        proxyPort = proxyPort;
    };

    # Let Home Manager install and manage itself.
    programs.home-manager.enable = true;

    # Packages
    home.packages = with pkgs; [ nixVersions.latest nixfmt-classic ];

    nix = {
        package = pkgs.nixVersions.latest;
        settings.experimental-features = [ "nix-command" "flakes" ];
        extraOptions = ''
      keep-outputs = true
      keep-derivations = true
        '';
    };

    # This value determines the Home Manager release that your
    # configuration is compatible with. This helps avoid breakage
    # when a new Home Manager release introduces backwards
    # incompatible changes.
    #
    # You can update Home Manager without changing this value. See
    # the Home Manager release notes for a list of state version
    # changes in each release.
    home.stateVersion = "20.09";
}
