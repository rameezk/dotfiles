{
  pkgs,
  lib,
  config,
  ...
}:

let
  cfg = config.vcs.git;

  extraSigningKeyModule = lib.types.submodule {
    options = {

      email = lib.mkOption {
        type = lib.types.str;
      };

      signingKey = lib.mkOption {
        type = lib.types.str;
      };

      paths = lib.mkOption {
        type = lib.types.listOf lib.types.str;
      };

    };
  };

  mkIncludeSettings = email: signingkey: path: {
    condition = "gitdir:" + path;
    contents = {
      user = {
        email = email;
        signingkey = signingkey;
      };
    };
  };

  forAllExtraSigningKeys =
    func: data: lib.concatMap (item: map (path: func item.email item.signingKey path) item.paths) data;

in
{
  options.vcs.git = {

    enable = lib.mkEnableOption "enable git";

    userName = lib.mkOption {
      type = lib.types.str;
      description = "User name to use.";
    };

    userEmail = lib.mkOption {
      type = lib.types.str;
      description = "User email to use.";
    };

    signingKey = lib.mkOption {
      type = lib.types.str;
      description = "Signing key to use.";
    };

    extraSigningKeys = lib.mkOption {
      type = lib.types.listOf extraSigningKeyModule;
      default = [ ];
    };

  };

  config = lib.mkIf cfg.enable {
    programs.git = {
      enable = true;
      userName = cfg.userName;
      userEmail = cfg.userEmail;
      aliases = {
        lg = "log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit";
        st = "status";
        br = "branch";
        dc = "diff --cached";
        d = "diff";
        co = "checkout";
        publish = "!git push --set-upstream origin $(git rev-parse --abbrev-ref HEAD)";
        delete-branches = ''!f() { git branch --merged | grep -v "master\|main" | xargs git branch -d; }; f'';
        prune-local-branches = "!f() { git branch -vv | grep ': gone' | awk '{print $1}' | fzf -m | xargs git branch -D; }; f";
        generate-ignore = ''!f() { curl -sL "https://www.gitignore.io/api/$1"; }; f'';
        open = "!f() { fish -c open_repo_in_browser; }; f";
        open-pr = "!f() { gh pr view --web; }; f";
        latest-tag = "!git tag --sort version:refname | tail -n 1";
        log-to-last-tag = "!f() { git log `git latest-tag`..HEAD --oneline; }; f";
        ignore-nix-shell = "!f() { grep -qxF 'shell.nix' `git rev-parse --show-toplevel`/.git/info/exclude || echo 'shell.nix' >> `git rev-parse --show-toplevel`/.git/info/exclude; }; f";
        rebase-commits-in-branch = "!f() { git rebase -i HEAD~$(git cherry -v main | wc -l | xargs); }; f";
        rb = "!f() { git checkout main; git pull; git checkout -; git rebase main; }; f";
        rc = "rebase-commits-in-branch";
        done = "!f() { git checkout main && git pull -p && git prune-local-branches ; }; f";
        sha = "rev-parse HEAD";
        create-gh-pr = "!f() { gh pr create -a @me ; }; f";
        copy-gh-pr-url = "!f() {  gh pr view --json url | jq -r '.url' | xargs echo -n | pbcopy; }; f";
        commit-into-previous = "commit --amend --no-edit";
      };
      includes = forAllExtraSigningKeys mkIncludeSettings cfg.extraSigningKeys;
      extraConfig = {
        core = {
          pager = "delta";
        };
        interactive = {
          diffFilter = "delta --color-only";
        };
        add.interactive = {
          useBuiltin = false;
        };
        delta = {
          navigate = true;
          light = false;
          side-by-side = true;
          hyperlinks = true;
          line-numbers = true;
          syntax-theme = "Catppuccin Frappe";
        };
        diff = {
          colorMoved = "default";
        };
        merge = {
          conflictstyle = "diff3";
        };
        user = {
          signingkey = cfg.signingKey;
        };
        commit = {
          gpgsign = true;
        };
        init = {
          defaultBranch = "main";
        };
        pull = {
          rebase = true;
        };
        fetch = {
          prune = true;
        };
        pager = {
          difftool = true;
        };
        rebase = {
          autoStash = true;
        };
      };
      ignores = [
        "*~"
        "*.swp"
        "*.orig"
        ".#*"
        ".direnv/"
        ".DS_Store"
        ".idea/"
      ];
    };

    programs.gpg = {
      enable = true;
    };

    home.packages = with pkgs; [
      git-crypt # encrypting git repos transparently
      pre-commit # a framework for dealing with git hooks
      delta # a better diffing tool pager
      cacert # needed for self signed certs in git
      gh # github cli
    ];
  };
}
