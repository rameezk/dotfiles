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

      # pipx
      set PATH $PATH /home/rameezk/.local/bin

      # Make gpg-agent play nicely with tmux
      export GPG_TTY=(tty)

      # dotfiles binaries
      set PATH $PATH /home/rameezk/.config/dotfiles/bin

      # source proxy
      source ~/.proxyrc

      # Fix gcc lib issues for certain python libs
      set -x LD_LIBRARY_PATH (nix eval --raw nixpkgs#stdenv.cc.cc.lib)/lib
    '';

    shellAbbrs = {
      # files
      ls = "exa";
      l = "exa -la --git";
      tree = "exa --tree";

      #git
      gcm = "git commit -m";
      gco = "git checkout";
      grt = "cd (git rev-parse --show-toplevel)";

      # home-manager
      hm-rm-old-generations =
        "home-manager generations | tail -n +2 | awk '{ print $5 }' | xargs home-manager remove-generations";

      # bat
      cat = "bat";

      # shell
      "reload!" = ''exec "$SHELL" -l'';

      # emacs
      e = "emacsclient --no-wait";

      # kubernetes
      k = "kubectl";
      kc = "kubectx";
      kn = "kubens";
      kw = "watch kubectl";
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
          set -l clone_to_path (echo "$repo_url" | sed 's/ssh:\/\/git@${secrets.git.work.e4m_base_url}/\/home\/rameezk\/code/' | sed 's/\.git//')
          git clone "$repo_url" "$clone_to_path"
        '';
      };

      mcd = {
        argumentNames = "directory";
        description = "create new directory and cd into it";
        body = ''
          mkdir -p "$directory" && cd "$directory";
        '';
      };

      k-exec-into = {
        description = "interactively select a pod and shell to exec into";
        body = ''
          set -l pod (kubectl get pods --no-headers | awk '{print $1}' | fzf)
          set -l shell (echo -e "bash\nsh\nzsh\nfish" | fzf)
          kubectl exec -it $pod -- $shell
        '';
      };

      k-delete-pods-with-status = {
        argumentNames = "pod_status";
        description = "delete pods with a specific status";
        body = ''
          if [ -z "$pod_status" ]
            echo "Please specify a status"
            return 1
          end

          kubectl get pods | grep -i "$pod_status" | awk '{print $1}' | xargs kubectl delete pod
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
    ignores = [ "*~" "*.swp" ".idea/" ];
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

  programs.emacs = {
    enable = true;
    package = pkgs.emacsGcc;
  };

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

    # fish plugins
    fishPlugins.pure
    fishPlugins.fzf-fish

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
