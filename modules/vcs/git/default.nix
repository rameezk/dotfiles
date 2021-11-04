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
      pr-complete = "!f() { git checkout master && git pull --prune; }; f";
      generate-ignore =
        ''!f() { curl -sL "https://www.gitignore.io/api/$1"; }; f'';
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
      user = { signingkey = secrets.user.work.gpgFingerprint; };
      commit = { gpgsign = true; };
    };
    ignores = [ "*~" "*.swp" ".idea/" "*.orig" ".#*" ];
  };

  home.packages = with pkgs; [
    git-crypt # encrypting git repos transparently
    pre-commit # a framework for dealing with git hooks
  ];
}
