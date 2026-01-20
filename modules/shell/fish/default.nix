{
  pkgs,
  lib,
  config,
  ...
}:
let
  proxyEnabled = config.network.proxy.enable or false;

  proxyConfig = lib.optionalString proxyEnabled ''
    source ~/.proxyrc
  '';

  baseShellInit = ''
    # Disable fish greeting message
    set fish_greeting

    # vi mode
    fish_vi_key_bindings

    # Enable fenv for sourcing foreign environment variables. 
    # This is needed for sourcing the Nix path below.
    set -p fish_function_path ${pkgs.fishPlugins.foreign-env}/share/fish/vendor_functions.d

    # nix
    if test -e ~/.nix-profile/etc/profile.d/nix.sh
      # Only source if the nix profile bin is not already in PATH
      if not contains ~/.nix-profile/bin $PATH
        fenv source ~/.nix-profile/etc/profile.d/nix.sh
      end
    end

    # Set LANG properly
    export LANG="en_ZA.UTF-8"

    # editor
    export EDITOR=vim

    # colorscheme
    # set -U fish_color_command b8bb26 # fish's default command color is a horrible dark blue, make it a nicer green

    # pipx
    fish_add_path -P ~/.local/bin

    # Make gpg-agent play nicely with tmux
    export GPG_TTY=(tty)

    # dotfiles binaries
    fish_add_path -P ~/.config/dotfiles/bin

    # Set ripgrep configuration file
    set -x RIPGREP_CONFIG_PATH ~/.ripgreprc

    # Set homebrew path
    fish_add_path -P /opt/homebrew/bin

    # Setup ASDF
    if test -e ~/.nix-profile/share/asdf-vm/asdf.fish
      source ~/.nix-profile/share/asdf-vm/asdf.fish
    end

    alias claude="/Users/rameezk/.claude/local/claude"
  '';

  shellInit = baseShellInit + proxyConfig;
in
{

  options = {
    fish.enable = lib.mkEnableOption "enable fish";
  };

  config = lib.mkIf config.fish.enable {
    programs.fish = {
      enable = true;

      shellInit = shellInit;

      shellAliases = {
        # containers
        docker = "podman";
        docker-compose = "podman-compose";
      };

      shellAbbrs = {
        # files
        ls = "eza";
        l = "eza -la --git";
        tree = "eza --tree";

        #git
        g = "git";
        gp = "git pull";
        gco = "git checkout";
        gcm = "git commit -m";
        grt = "cd (git rev-parse --show-toplevel)";
        gs = "git status";

        # home-manager
        hm-rm-old-generations = "home-manager generations | tail -n +2 | awk '{ print $5 }' | xargs home-manager remove-generations";

        # bat
        cat = "bat";

        # shell
        "reload!" = ''exec "$SHELL" -l'';

        # kubernetes
        k = "kubectl";
        kc = "kubectx";
        kn = "kubens";
        kw = "watch kubectl";
        kdebug = "kubectl run --rm -i -t debug --image=rameezk/debuggery --restart=Never";

        # Nix
        "," = "nix run nixpkgs#";

        # terraform and terrgrunt
        tf = "terraform";
        tg = "terragrunt";
      };

      functions = {
        open_repo_in_browser = {

          description = "open a git remote in default browser";
          body = ''
            set -l git_remote (git remote -v | head -n 1 | awk '{print $2}')

            if string match -r '^git@.*$' $git_remote
              set browser_remote (echo $git_remote | sed 's#:#/#' | sed 's#git@#http://#' | sed -E 's#.git$##')
            else
              set browser_remote (echo $git_remote | sed 's/.*@/http:\/\//' | sed -E 's/:[0-9]+//g' | sed 's/\.com:/\.com\//' | sed 's/\.[^.]*$//')
            end

            echo "[..] Opening $browser_remote in default browser"
            open "$browser_remote" > /dev/null 2>&1
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

        can_i_haz_internetz_plez = {
          description = "check if connected to internet";
          body = ''
            set -l status_code (curl -s -o /dev/null -w "%{http_code}" https://www.ipecho.net/plain)

            if [ "$status_code" = 200 ]
              echo "       Yes"
              echo "  |\---/|
              | o_o |
               \_^_/"
              return 0
            else
              echo "       No"
              echo "  |\---/|
              | o_o |
               \_^_/"
              return 1
            end
          '';
        };

        generate_uuid4 = {
          description = "generate a uuid4";
          body = ''
            python -c "import uuid; print(str(uuid.uuid4()).strip(), end=\"\");"
          '';
        };
      };

      plugins = [
        {
          name = "fzf-fish";
          src = pkgs.fishPlugins.fzf-fish.src;
        }
      ];

    };

  };

}
