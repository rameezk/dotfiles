{ pkgs, ... }:

let secrets = import ../../../secrets/config.nix;
in {
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
        !f() { git branch --merged | grep -v "master\|main" | xargs git branch -d; }; f'';
      prune-local-branches =
        "!f() { git branch -vv | grep ': gone' | awk '{print $1}' | fzf -m | xargs git branch -D; }; f";
      generate-ignore =
        ''!f() { curl -sL "https://www.gitignore.io/api/$1"; }; f'';
      open = "!f() { fish -c open_repo_in_browser; }; f";
      latest-tag = "!git tag --sort version:refname | tail -n 1";
      log-to-last-tag = "!f() { git log `git latest-tag`..HEAD --oneline; }; f";
      ignore-nix-shell =
        "!f() { grep -qxF 'shell.nix' `git rev-parse --show-toplevel`/.git/info/exclude || echo 'shell.nix' >> `git rev-parse --show-toplevel`/.git/info/exclude; }; f";
      rebase-commits-in-branch =
        "!f() { git rebase -i HEAD~$(git cherry -v main | wc -l | xargs); }; f";
      sha = "rev-parse HEAD";
    };
    includes = [
      {
        condition = "gitdir:${secrets.git.work.base_repo_dir}";
        contents = { credential = { helper = "cache --timeout 18000"; }; };
      }
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
      core = { pager = "delta"; };
      interactive = { diffFilter = "delta --color-only"; };
      add.interactive = { useBuiltin = false; };
      delta = {
        navigate = true;
        light = false;
        side-by-side = true;
        hyperlinks = true;
        line-numbers = true;
        syntax-theme = "Dracula";
      };
      diff = { colorMoved = "default"; };
      merge = { conflictstyle = "diff3"; };
      user = { signingkey = secrets.user.work.gpgFingerprint; };
      commit = { gpgsign = true; };
      init = { defaultBranch = "main"; };
      pull = { rebase = false; };
      fetch = { prune = true; };
      pager = { difftool = true; };
      http."${secrets.git.work.base_url}" = {
        sslCAInfo = secrets.git.work.ssl_ca_info;
      };
    };
    ignores = [ "*~" "*.swp" "*.orig" ".#*" ".direnv/" ".DS_Store" ];
  };

  programs.gpg = { enable = true; };

  # home.file.".gnupg/pinentry-switcher".source =
  #   pkgs.writeShellScript "pinentry-switcher" ''
  #     case $PINENTRY_USER_DATA in
  #       emacs)
  #         exec ~/.nix-profile/bin/pinentry-emacs "$@"
  #         ;;
  #       none)
  #         exit 1
  #         ;;
  #       *)
  #         exec pinentry-curses "$@"
  #     esac
  #   '';

  # home.file.".gnupg/gpg-agent.conf".text = ''
  #   pinentry-program ~/.gnupg/pinentry-switcher
  #   default-cache-ttl 18000
  #   max-cache-ttl 18000
  #   enable-ssh-support
  #   allow-emacs-pinentry
  #   allow-loopback-pinentry
  # '';

  # home.file.".gnupg/gpg.conf".text = ''
  #   use-agent
  # '';

  home.packages = with pkgs; [
    git-crypt # encrypting git repos transparently
    pre-commit # a framework for dealing with git hooks
    delta # a better diffing tool pager
    cacert # needed for self signed certs in git
    gh # github cli
  ];
}
