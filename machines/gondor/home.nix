{ pkgs, ... }:

let
  proxyProtocol = "http";
  proxyHost = "localhost";
  proxyPort = 3128;

  secrets = import ../../secrets/config.nix;
in
{
  imports = [
    ../../modules
  ];

  network.proxy = {
    enable = true;
    protocol = proxyProtocol;
    host = proxyHost;
    port = proxyPort;
  };

  cloud.aws.enable = true;

  shell.enable = true;

  vcs.git = {
    enable = true;
    userName = "Rameez Khan";
    userEmail = secrets.user.work.emailAddr;
    signingKey = "FB1E24B0094A24D8";
    extraSigningKeys = [
      {
        email = "rameezkhan.sa@gmail.com";
        signingKey = "FA38EE525EC823AF";
        paths = [
          "~/code/personal/"
          "~/.config/"
        ];
      }
    ];
  };

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

  macos.window-management.enable = true;

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Other Packages
  home.packages = with pkgs; [
    nixVersions.latest
    asdf-vm
  ];

  nix = {
    package = pkgs.nixVersions.latest;
    settings.experimental-features = [
      "nix-command"
      "flakes"
    ];
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
