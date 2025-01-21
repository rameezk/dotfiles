{ pkgs, ... }:

let
  user = "rameezk";
in
{
  services.nix-daemon.enable = true;

  nix.settings = {
    experimental-features = "nix-command flakes";
  };

  nix.optimise.automatic = true;

  nix.gc = {
    automatic = true;
    interval = {
      Weekday = 3;
      Hour = 8;
      Minute = 0;
    };
    options = "--delete-older-than 14d";
  };

  users.users.rameezk = {
    home = "/Users/rameezk";
    shell = pkgs.fish;
    uid = 501;
  };

  users.knownUsers = [ user ];

  programs.fish.enable = true;

  # enable touchID for sudo
  security.pam.enableSudoTouchIdAuth = true;
  environment = {
    etc."pam.d/sudo_local".text = ''
      # Managed by Nix Darwin
      auth       optional       ${pkgs.pam-reattach}/lib/pam/pam_reattach.so ignore_ssh
      auth       sufficient     pam_tid.so
    '';
  };

  system.defaults = {
    dock = {
      autohide = true;
      show-recents = false;
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
      "alfred"
      "claude"
      "clop"
      "docker"
      "elgato-control-center"
      "google-chrome"
      "intellij-idea"
      "jabra-direct"
      "keymapp"
      "microsoft-outlook"
      "microsoft-teams"
      "nikitabobko/tap/aerospace"
      "obsidian"
      "proton-mail"
      "slack"
      "todoist"
      "wezterm"
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
      "1password" = 1333542190;
      "adguard" = 1440147259;
      "dato" = 1470584107;
    };
  };

}
