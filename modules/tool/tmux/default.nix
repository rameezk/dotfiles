{ pkgs, ... }: {

  programs.tmux = {
    enable = true;

    clock24 = true; # Use a 24 hour clock
    baseIndex = 1; # Start window count at 1
    historyLimit = 100000; # max lines held in history
    keyMode = "vi"; # use VI keybindings
    prefix = "C-Space"; # Use CTRL-SPACE as prefix
    terminal = "screen-256color";

    extraConfig = ''
      set -g set-titles on
      set -g set-titles-string '#(whoami)::#h'
      set -g status-interval 1 # quicker status line updates
      set -s escape-time 0 # VI escape is instant
      setw -g mouse on # Turn on mouse mode
      set-option -g xterm-keys on # Enable modifier keys

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
      sessionist
      {
        plugin = yank;
        extraConfig = "set -g @yank_selection_mouse 'clipboard'";
      }
      {
        plugin = dracula;
        extraConfig = ''
          set -g @dracula-plugins "battery cpu-usage ram-usage time"
          set -g @dracula-show-powerline true
          set -g @dracula-show-flags true
          set -g @dracula-border-contrast true
          set -g @dracula-show-left-icon "❐ #S#{prefix_highlight}#{?window_zoomed_flag, 🔍,}"
          set -g @dracula-show-timezone false
          set -g @dracula-military-time true
          set -g @dracula-show-left-sep 
          set -g @dracula-show-right-sep 
          set -g @dracula-cpu-usage-label ﬙
          set -g @dracula-ram-usage-label 
          set -g @dracula-battery-label 
        '';
      }
    ];
  };
}
