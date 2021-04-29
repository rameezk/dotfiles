{ config, pkgs, lib, ... }:

let
  username = "rameezk";
  homeDirectory = "/home/rameezk";

  secrets = import ./secrets/config.nix;
in {
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = username;
  home.homeDirectory = homeDirectory;

  # shell
  programs.fish = {
    enable = true;

    shellInit = ''
      # Disable fish greeting message
      set fish_greeting

      # vi mode
      fish_vi_key_bindings

      # Enable fenv for sourcing foreign environment variables. 
      # This is needed for sourcing the Nix path below.
      set -p fish_function_path ${pkgs.fishPlugins.foreign-env}/share/fish/vendor_functions.d

      # nix
      if test -e ${homeDirectory}/.nix-profile/etc/profile.d/nix.sh
        fenv source ${homeDirectory}/.nix-profile/etc/profile.d/nix.sh
      end

      # editor
      export EDITOR=vim

      # colorscheme
      set -U fish_color_command b8bb26 # fish's default command color is a horrible dark blue, make it a nicer green

      # direnv
      eval (direnv hook fish)

      # asdf
      source ~/.asdf/asdf.fish

      # pipx
      set PATH $PATH /home/rameezk/.local/bin

      # Make gpg-agent play nicely with tmux
      export GPG_TTY=(tty)

      # dotfiles binaries
      set PATH $PATH /home/rameezk/.config/dotfiles/bin

      # source proxy
      source ~/.proxyrc
    '';

    shellAbbrs = {
      # files
      ls = "exa";
      l = "exa -la --git";
      tree = "exa --tree";

      #git
      gcm = "git commit -m";
      gco = "git checkout";

      # home-manager
      hm-rm-old-generations =
        "home-manager generations | tail -n +2 | awk '{ print $5 }' | xargs home-manager remove-generations";

      # bat
      cat = "bat";
    };

    functions = {
      p = {
        argumentNames = "args";
        description = "open fzf to cd into a project directory";
        body = ''
          set -l cache_file "$HOME/.projects"
          if test -z $args
              if test -s "$cache_file"
                  set -l chosen_dir (cat $cache_file | fzf)
                  cd "$chosen_dir"
              else
                  bash -c "find ~/code -name .git -type d -prune | xargs readlink -f | xargs dirname > $cache_file"
                  set -l chosen_dir (cat $cache_file | fzf)
                  cd "$chosen_dir"
              end
          else
              if [ $args = "-r" ]
                  bash -c "find ~/code -name .git -type d -prune | xargs readlink -f | xargs dirname > $cache_file"
                  set -l chosen_dir (cat $cache_file | fzf)
                  cd "$chosen_dir"
              end
          end
        '';
      };

      clone_e4m_repo = {
        argumentNames = "repo_url";
        description = "clone e4m repo to correct directory";
        body = ''
          set -l clone_to_path (echo "$repo_url" | sed 's/ssh:\/\/git@${secrets.git.work.e4m_base_url}/\/home\/rameezk\/code/')
          git clone "$repo_url" "$clone_to_path"
        '';
      };

    };

    plugins = [{
      name = "z";
      src = pkgs.fetchFromGitHub {
        owner = "jethrokuan";
        repo = "z";
        rev = "d5500284077ebb12c306ea429e74c8d046aef5a0";
        sha256 = "sha256-I2feYLp+oqVGjtaG5uftG0Lok5ye7G8oefZAMdzAeoo=";
      };
    }];
  };

  # git
  programs.git = {
    enable = true;
    userName = secrets.user.fullName;
    userEmail = secrets.user.work.emailAddr;
    aliases = {
      lg =
        "log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit";
      st = "status";
      dc = "diff --cached";
      d = "diff";
      publish =
        "!git push --set-upstream origin $(git rev-parse --abbrev-ref HEAD)";
      delete-branches = ''
        !f() { git branch | grep -v "master\|main" | xargs git branch -d; }; f'';
      pr-complete = "!f() { git checkout master && git pull --prune; }; f";
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
    ignores = [ "*~" "*.swp" ];
  };

  # vim
  programs.vim = {
    enable = true;
    plugins = with pkgs.vimPlugins; [
      vim-sensible
      vim-airline
      vim-commentary
      gruvbox
    ];
    extraConfig = ''
      set background=dark " Better colours for darker backgrounds
      set number "Turn on line numbers
      set cursorline " Highlight the current line the cursor is on
      set visualbell " Turn on visual bell
      colorscheme gruvbox " Turn on gruvbox colorscheme
    '';
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
    # cli tools
    ripgrep # faster grepping
    tmux # terminal multiplexor
    jq # parsing JSON
    entr # file watching
    fzf # fuzzy file finder
    htop # process manager
    neofetch # nice system info viewer

    # git
    git
    git-crypt # encrypting git repos transparently

    # cloud
    azure-cli

    # kubernetes
    kubectl
    kubectx
    stern

    # fish plugins
    fishPlugins.pure

    # emacs dependencies
    sqlite # needed for org-roam
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
