{ pkgs, ... }:

let secrets = import ../../../secrets/config.nix;
in {

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
      if test -e ~/.nix-profile/etc/profile.d/nix.sh
        fenv source ~/.nix-profile/etc/profile.d/nix.sh
      end

      # Set LANG properly
      export LANG="en_ZA.utf8"

      # editor
      export EDITOR=vim

      # colorscheme
      set -U fish_color_command b8bb26 # fish's default command color is a horrible dark blue, make it a nicer green

      # direnv
      # eval (direnv hook fish)
      ${pkgs.direnv}/bin/direnv hook fish | source

      # pipx
      set PATH $PATH ~/.local/bin

      # Make gpg-agent play nicely with tmux
      export GPG_TTY=(tty)

      # dotfiles binaries
      set PATH $PATH ~/.config/dotfiles/bin

      # source proxy
      source ~/.proxyrc

      # Fix gcc lib issues for certain python libs
      # Only needed on WSL Ubuntu for some reason
      # set -x LD_LIBRARY_PATH /nix/store/9ilyrqidrjbqvmnn8ykjc7lygdd86g7q-gcc-10.2.0-lib/lib:/nix/store/1l4r0r4ab3v3a3ppir4jwiah3icalk9d-zlib-1.2.11/lib
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
      kdebug =
        "kubectl run --rm -i -t debug --image=rameezk/debuggery --restart=Never";
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
          set -l clone_to_path (echo "$repo_url" | sed 's#https://${secrets.git.work.e4m_base_url}#${secrets.git.work.code_directory}#' | sed 's/\.git//')
          git clone "$repo_url" "$clone_to_path"
          cd "$clone_to_path"
        '';
      };

      open_repo_in_browser = {

        description = "open a git remote in default browser";
        body = ''
          if [ ! -d ".git" ];
            echo "[..] Not a git repo"
            return 1
          end

          set -l git_remote (git remote -v | head -n 1 | awk '{print $2}')

          set -l browser_remote (echo $git_remote | sed 's/.*@/http:\/\//' | sed -E 's/:[0-9]+//g' | sed 's/\.com:/\.com\//' | sed 's/\.[^.]*$//')

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

      aws-switch-role = {
        description = "switch aws roles";
        body = ''
          set config_file "$HOME/.aws/aws-role-mappings.json"

          if [ ! -s $config_file ];
             echo "[..] Missing $config_file file"
             return 1
          end


          set CHOSEN_ROLE (cat "$config_file" | jq -r ".roles | .[] | .text_to_display" | fzf)
          set CHOSEN_ACCOUNT_ID_NUMBER (cat "$config_file" | jq -r ".roles | .[] | select(.text_to_display==\"$CHOSEN_ROLE\") | .account_id_number")
          set CHOSEN_ROLE_NAME (cat "$config_file" | jq -r ".roles | .[] | select(.text_to_display==\"$CHOSEN_ROLE\") | .role_name")

          if [ -z "$CHOSEN_ROLE" ];
          	echo "[..] No CHOSEN_ROLE defined"
          	return 1
          end
          if [ -z "$CHOSEN_ACCOUNT_ID_NUMBER" ];
          	echo "[..] No CHOSEN_ACCOUNT_ID_NUMBER defined"
          	return 1
          end
          if [ -z "$CHOSEN_ROLE_NAME" ];
          	echo "[..] No CHOSEN_ROLE_NAME defined"
          	return 1
          end

          open "https://signin.aws.amazon.com/switchrole?account=$CHOSEN_ACCOUNT_ID_NUMBER&roleName=$CHOSEN_ROLE_NAME&displayName=$CHOSEN_ROLE"
          echo "[..] Opened in browser"
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

  home.packages = with pkgs; [ fishPlugins.pure fishPlugins.fzf-fish ];

}
