{ pkgs, ... }:

let
  user = "rameezk";
in
{
  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;
  # nix.package = pkgs.nix;

  # Necessary for using flakes on this system.
  nix.settings = {
    experimental-features = "nix-command flakes";
    auto-optimise-store = true;
  };

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

  # Create /etc/zshrc that loads the nix-darwin environment.
  # programs.zsh.enable = true;  # default shell on catalina
  programs.fish.enable = true;

  # Set Git commit hash for darwin-version.
  # system.configurationRevision = self.rev or self.dirtyRev or null;

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
      "chatgpt"
      "google-chrome"
      "obsidian"
      "proton-mail"
      "slack"
      "todoist"
      "wezterm"
      "zoom"
      "nikitabobko/tap/aerospace"
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
    };
  };

}
