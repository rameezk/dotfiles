{ pkgs, auto-volume-toggler, ... }:

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

    onActivation = {
      autoUpdate = true;
      upgrade = true;
      cleanup = "none";
    };

    brews = [ "mas" ];
    casks = import ./casks.nix;

    # These app IDs are from using the mas CLI app
    # mas = mac app store
    # https://github.com/mas-cli/mas
    #
    # $ nix shell nixpkgs#mas
    # $ mas search <app name>
    #
    masApps = {
      "AdGuard Mini: Safari Adblock" = 1440147259;
      "dato" = 1470584107;
      "tuneful" = 6739804295;
    };
  };

  launchd.user.agents.auto-volume-toggler =
    let
      pkg = auto-volume-toggler.packages.aarch64-darwin.auto-volume-toggler {
        targetVolume = 55;
        deviceName = "MacBook Pro Speakers";
      };
    in
    {
      serviceConfig = {
        ProgramArguments = [
          "/bin/sh"
          "-c"
          ''echo "$(date '+%Y-%m-%d %H:%M:%S') - Running auto-volume-toggler" && ${pkg}/bin/auto-volume-toggler 2>&1''
        ];
        StartInterval = 300; # 5 minutes
        StandardOutPath = "/tmp/auto-volume-toggler.log";
        StandardErrorPath = "/tmp/auto-volume-toggler.log";
      };
    };

  launchd.user.agents.msteams-cleanup = {
    serviceConfig = {
      ProgramArguments = [
        "/bin/sh"
        "-c"
        ''
          DIR="$HOME/Downloads/MSTeams"
          if [ -d "$DIR" ]; then
            SIZE=$(du -sh "$DIR" 2>/dev/null | cut -f1)
            rm -rf "$DIR"
            echo "$(date '+%Y-%m-%d %H:%M:%S') - Deleted $DIR ($SIZE)"
          else
            echo "$(date '+%Y-%m-%d %H:%M:%S') - $DIR does not exist, skipping"
          fi
        ''
      ];
      RunAtLoad = true;
      StartInterval = 1800; # Every 30 min
      StandardOutPath = "/tmp/msteams-cleanup.log";
      StandardErrorPath = "/tmp/msteams-cleanup.log";
    };
  };

}
