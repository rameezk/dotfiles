{ pkgs, ... }:

let
  user = "rameezk";
in
{

  nix = {
    enable = false; # # let determinate installer manage the nix daemon
    settings.experimental-features = "nix-command flakes";
  };

  users.users.rameezk = {
    home = "/Users/${user}";
    shell = pkgs.fish;
    uid = 501;
  };

  users.knownUsers = [ user ];

  programs.fish.enable = true;

  # enable touchID for sudo
  security.pam.services.sudo_local.touchIdAuth = true;
  environment = {
    etc."pam.d/sudo_local".text = ''
      # Managed by Nix Darwin
      auth       optional       ${pkgs.pam-reattach}/lib/pam/pam_reattach.so ignore_ssh
      auth       sufficient     pam_tid.so
    '';
  };

  system.primaryUser = user;

  system.defaults = {
    dock = {
      autohide = true;
      show-recents = false;
      mru-spaces = false;
    };

    # Might require you to logout to take effect
    NSGlobalDomain = {
      "com.apple.swipescrolldirection" = false;
      "com.apple.keyboard.fnState" = true;

      # 120, 90, 60, 30, 12, 6, 2
      KeyRepeat = 2;

      # 120, 94, 68, 35, 25, 15
      InitialKeyRepeat = 15;
    };
  };

  system.keyboard = {
    enableKeyMapping = true;
    remapCapsLockToEscape = true;
  };

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;

  # The platform the configuration will be used on.
  nixpkgs.hostPlatform = "aarch64-darwin";

  homebrew = {
    # This is a module from nix-darwin
    # Homebrew is *installed* via the flake input nix-homebrew
    enable = true;

    casks = [
      "1password"
      "1password-cli"
      "alfred"
      "claude"
      "clop"
      "cursor"
      "deskpad"
      "elgato-control-center"
      "google-chrome"
      "insomnia"
      "intellij-idea"
      "jabra-direct"
      "karabiner-elements"
      "keymapp"
      "microsoft-azure-storage-explorer"
      "microsoft-excel"
      "microsoft-outlook"
      "microsoft-powerpoint"
      "microsoft-teams"
      "microsoft-word"
      "obsidian"
      "podman-desktop"
      "proton-mail"
      "protonvpn"
      "slack"
      "spotify"
      "stats"
      "todoist-app"
      "visual-studio-code"
      "wezterm"
      "wireshark-app"
      "zoom"
    ];

    # These app IDs are from using the mas CLI app
    # mas = mac app store
    # https://github.com/mas-cli/mas
    #
    # $ nix shell nixpkgs#mas
    # $ mas search <app name>
    #
    masApps = {
      "adguard" = 1440147259;
      "dato" = 1470584107;
      "tuneful" = 6739804295;
    };
  };

}
