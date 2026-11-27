{ inputs, pkgs, ... }:

{
  imports = [
    ../../modules
    inputs.jarvis.homeManagerModules.default
    inputs.forge.homeManagerModules.default
  ];

  programs.forge-mcp.boxAddress = "https://forge.tail29474.ts.net";

  sops = {
    defaultSopsFile = "${builtins.toString inputs.mysecrets}/secrets.yaml";
    validateSopsFiles = false;

    age.keyFile = "/Users/rameezk/.config/sops/age/keys.txt";

    defaultSecretsMountPoint = "/Users/rameezk/.config/sops-nix/secrets.d";

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

  verify.checks = [
    {
      type = "file";
      path = "~/.config/sops/age/keys.txt";
      desc = "SOPS age keyfile";
    }
    {
      type = "ssh_key";
      path = "~/.ssh/id_ed25519";
      desc = "SSH private key valid";
    }
    {
      type = "file_permissions";
      path = "~/.ssh/id_ed25519";
      permissions = "600";
      desc = "SSH key permissions";
    }
    {
      type = "gpg_key";
      path = "~/.config/gpg/rameezk_private.gpg";
      desc = "GPG private key valid";
    }
  ];

  theme.rose-pine = {
    enable = true;
    flavour = "moon";
    lightFlavour = "dawn";
  };

  ai.enable = false;
  ai.agents.enable = true;
  ai.claude.enable = true;
  ai.claude.configDirs = [
    ".claude"
    ".claude-work"
  ];
  ai.pi.enable = true;
  ai.claude-skills.docx.enable = false;
  ai.claude-skills.drawio.enable = false;
  ai.claude-skills.herdr.enable = true;
  ai.claude-skills.mermaid.enable = false;

  macos.alfred.enable = true;
  macos.hammerspoon.enable = true;
  macos.karabiner.enable = true;
  macos.window-management.aerospace.enable = true;

  fonts.enable = true;

  shell.enable = true;

  herdr.enable = true;

  herdr.app.enable = true;

  editor = {
    neovim.enable = true;
    jetbrains-vim-mode.enable = true;
  };

  vcs.git = {
    enable = true;
    userName = "Rameez Khan";
    userEmail = "5321924+rameezk@users.noreply.github.com";
    signingKey = "B18F680AF21C6B4A";
  };

  vcs.hunk = {
    enable = true;
    settings = {
      wrap_lines = true;
      agent_notes = true;
      transparent_background = true;
      color_moved = true;
    };
  };

  programs.ssh = {
    enable = true;
    enableDefaultConfig = false;
    settings."github.com" = {
      IdentityFile = "~/.ssh/id_ed25519";
      AddKeysToAgent = "yes";
    };
  };

  home.file.".ssh/id_ed25519.pub".source = ../../keys/id_ed25519.pub;

  language = {
    python.enable = true;
    java = {
      enable = true;
      manageJDK = false;
    };
    nodejs.enable = true;
  };

  cloud = {
    aws.enable = true;
    azure.enable = true;
  };

  container = {
    enable = true;
    podman = true;
  };

  iac.terraform.enable = true;

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Packages
  home.packages = with pkgs; [
    inputs.mdmarks.packages.${pkgs.stdenv.hostPlatform.system}.default
    inputs.jarvis.packages.${pkgs.stdenv.hostPlatform.system}.jarvis
    inputs.jarvis.packages.${pkgs.stdenv.hostPlatform.system}.jarvis-mail
    inputs.jarvis.packages.${pkgs.stdenv.hostPlatform.system}.jarvis-digital-garden
    inputs.jarvis.packages.${pkgs.stdenv.hostPlatform.system}.jarvis-calendar
  ];

  programs.jarvis.calendar = {
    enable = true;
    blockingCalendar = "Personal";
  };

  programs.jarvis.digitalGarden = {
    enable = true;
    vault = "/Users/rameezk/DigitalGarden";
  };

  programs.jarvis.mail = {
    enable = true;
    tlsFingerprint = "BE:88:19:4B:17:4E:21:D4:3A:5C:BC:DD:39:ED:98:C5:F8:3E:B0:73:F9:7A:65:D7:50:09:F0:44:06:2E:E4:88";
    onepassword = {
      account = "my.1password.com";
      usernameRef = "op://private/Proton Mail Bridge/username";
      passwordRef = "op://private/Proton Mail Bridge/password";
    };
  };

  xdg.configFile."mdmarks/config.toml".text = ''
    store = "/Users/rameezk/DigitalGarden/bookmarks"
    default_space = "personal"

    [spaces.personal]
    browser = "Helium"
    profile = "Personal"
    chromium_support_dir = "net.imput.helium"

    [spaces.ee]
    browser = "Helium"
    profile = "EE"
    chromium_support_dir = "net.imput.helium"
  '';

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
