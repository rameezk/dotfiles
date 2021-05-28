{ config, pkgs, lib, ... }:

let
  username = "rameezk";
  homeDirectory = "/home/rameezk";

  secrets = import ./secrets/config.nix;
in {
  # Opt-in to modules by including theme here.
  #  Is there a better way to do this per machine?
  imports =
    [ ./modules/editors/emacs ./modules/editors/vim ./modules/shell/fish ];

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = username;
  home.homeDirectory = homeDirectory;

  # git
  programs.git = {
    enable = true;
    userName = secrets.user.fullName;
    userEmail = secrets.user.work.emailAddr;
    aliases = {
      lg =
        "log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit";
      st = "status";
      br = "branch";
      dc = "diff --cached";
      d = "diff";
      co = "checkout";
      publish =
        "!git push --set-upstream origin $(git rev-parse --abbrev-ref HEAD)";
      delete-branches = ''
        !f() { git branch | grep -v "master\|main" | xargs git branch -d; }; f'';
      pr-complete = "!f() { git checkout master && git pull --prune; }; f";
      generate-ignore =
        ''!f() { curl -sL "https://www.gitignore.io/api/$1"; }; f'';
    };
    includes = [
      {
        condition = "gitdir:~/code/personal/";
        contents = {
          user = {
            email = secrets.user.personal.emailAddr;
            signingkey = secrets.user.personal.gpgFingerprint;
          };
        };
      }
      {
        condition = "gitdir:~/.config/";
        contents = {
          user = {
            email = secrets.user.personal.emailAddr;
            signingkey = secrets.user.personal.gpgFingerprint;
          };
        };
      }
    ];
    extraConfig = {
      user = { signingkey = secrets.user.work.gpgFingerprint; };
      commit = { gpgsign = true; };
    };
    ignores = [ "*~" "*.swp" ".idea/" "*.orig" ".#*" ];
  };

  # bat
  programs.bat = {
    enable = true;
    config = {
      theme = "gruvbox-dark";
      pager = "less -FR";
    };
  };

  # exa
  programs.exa.enable = true;

  # Packages
  home.packages = with pkgs; [
    nixUnstable

    # nix
    nixfmt

    # cli tools
    ripgrep # faster grepping
    tmux # terminal multiplexor
    jq # parsing JSON
    entr # file watching
    fzf # fuzzy file finder
    htop # process manager
    neofetch # nice system info viewer
    fd # a faster find

    # git
    git
    git-crypt # encrypting git repos transparently
    pre-commit # a framework for dealing with git hooks

    # cloud
    azure-cli

    # kubernetes
    kubectl
    kubectx
    stern

    # emacs dependencies
    sqlite # needed for org-roam

    # markdown
    multimarkdown

    # spell checkiing
    aspell
    aspellDicts.en
    aspellDicts.en-computers
    aspellDicts.en-science

    # lang
    ## python
    python39
    stdenv.cc.cc.lib
    python39Packages.pip
    python39Packages.pipx

    # media
    ffmpeg

    # clojure
    clojure
    clj-kondo
  ];

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
