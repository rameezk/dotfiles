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

      # Set ripgrep configuration file
      set -x RIPGREP_CONFIG_PATH ~/.ripgreprc

      # Set PyCharm path on MacOS
      set PATH $PATH /Applications/PyCharm.app/Contents/MacOS

      # Setup ASDF
      if test -e ~/.nix-profile/share/asdf-vm/asdf.fish
        source ~/.nix-profile/share/asdf-vm/asdf.fish
      end
    '';

    shellAbbrs = {
      # files
      ls = "eza";
      l = "eza -la --git";
      tree = "eza --tree";

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

      # Nix
      "," = "nix run nixpkgs#";
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

  programs.starship = {
    enable = true;
    enableFishIntegration = true;
    settings = { docker_context = { disabled = true; }; };
  };

  home.packages = with pkgs; [ fishPlugins.fzf-fish fishPlugins.bass ];

}
