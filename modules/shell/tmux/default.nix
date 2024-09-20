{
  pkgs,
  lib,
  config,
  ...
}:
let
  cfg = config.tmux;

  catppuccinThemeEnabled = config.theme.catppuccin.enable or false;
in
{

  options.tmux = {
    enable = lib.mkEnableOption "enable tmux";
  };

  config = lib.mkIf cfg.enable {

    programs.tmux = {
      enable = true;

      catppuccin = lib.mkIf catppuccinThemeEnabled {
        enable = true;
        extraConfig = ''
          set -g @catppuccin_window_right_separator "█ "
          set -g @catppuccin_window_number_position "right"
          set -g @catppuccin_window_middle_separator " | "

          set -g @catppuccin_window_default_fill "none"

          set -g @catppuccin_window_current_fill "all"

          set -g @catppuccin_status_modules_right "date_time session"

          set -g @catppuccin_status_left_separator "█"
          set -g @catppuccin_status_right_separator "█"

          set -g @catppuccin_date_time_text "%a %d/%m %H:%M"
        '';
      };

      clock24 = true; # Use a 24 hour clock
      baseIndex = 1; # Start window count at 1
      historyLimit = 100000; # max lines held in history
      keyMode = "vi"; # use VI keybindings
      prefix = "C-Space"; # Use CTRL-SPACE as prefix
      terminal = "tmux-256color";

      extraConfig = ''
        set -ga terminal-overrides ",*256col*:Tc" # fix colours

        set -g set-titles on
        set -g set-titles-string '#(whoami)::#h'
        set -g status-interval 1 # quicker status line updates
        set -s escape-time 0 # VI escape is instant
        setw -g mouse on # Turn on mouse mode
        set-option -g xterm-keys on # Enable modifier keys

        # Add two status lines, one blank to emulate white space. To provide some spacing above status line.
        set-option -g status-position bottom
        setw -g pane-border-status bottom
        # setw -g pane-border-format '#[fg=colour2] $ #{pane_current_command} '
        set-option -g pane-active-border-style fg=blue

        # Visual Notifications
        setw -g monitor-activity on 
        set -g visual-activity on

        # Reload config via keybinding
        bind r source-file ~/.config/tmux/tmux.conf \; display-message "Config reloaded."

        # Use Shift-arrow keys for switching windows
        bind -n S-Left  previous-window
        bind -n S-Right next-window

        # Window management
        unbind-key -
        bind - split-window -v -c "#{pane_current_path}"
        unbind-key |
        bind | split-window -h -c "#{pane_current_path}"

        unbind-key j
        bind-key j select-pane -D
        unbind-key k
        bind-key k select-pane -U
        unbind-key h
        bind-key h select-pane -L
        unbind-key l
        bind-key l select-pane -R

        # Fuzzy Find Sessions
        bind C-j split-window -v "tmux list-sessions | sed -E 's/:.*$//' | fzf --reverse | xargs tmux switch-client -t"

        # Send command to all panes in all sessions
        bind E command-prompt -p "Command:" \
               "run \"tmux list-panes -a -F '##{session_name}:##{window_index}.##{pane_index}' \
                      | xargs -I PANE tmux send-keys -t PANE '%1' Enter\""

        # Sync panes
        bind e set-window-option synchronize-panes \; display-message "Toggled pane synchronisation"
      '';

      plugins = with pkgs.tmuxPlugins; [
        {
          plugin = yank;
          extraConfig = "set -g @yank_selection_mouse 'clipboard'";
        }
      ];
    };

    home.file."boot-tmux.sh" = {
      executable = true;
      text = ''
        #!/usr/bin/env bash

        echo "[..] Booting tmux"
        tmux attach -t base || tmux new -s base
      '';
    };
  };
}
