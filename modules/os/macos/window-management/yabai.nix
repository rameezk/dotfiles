{
  pkgs,
  lib,
  config,
  ...
}:
let
  cfg = config.macos.window-management.yabai;
in
{
  options.macos.window-management.yabai = {
    enable = lib.mkEnableOption "enable yabai";
  };

  config = lib.mkIf cfg.enable {
    home.file.yabai = {
      executable = true;
      target = ".config/yabai/yabairc";
      text = ''
        sudo yabai --load-sa
        yabai -m signal --add event=dock_did_restart action="sudo yabai --load-sa"
        yabai -m signal --add event=window_focused action="sketchybar --trigger window_focus"
        yabai -m signal --add event=display_added action="sleep 2 && $HOME/.dotfiles/yabai/move_spaces_multiple_screens.sh"
        yabai -m signal --add event=display_removed action="sleep 1 && $HOME/.dotfiles/yabai/move_spaces_single_screen.sh"
        yabai -m signal --add event=window_created action="sketchybar --trigger windows_on_spaces"
        yabai -m signal --add event=window_destroyed action="sketchybar --trigger windows_on_spaces"

        $HOME/.config/yabai/create_spaces.sh

        # layout
        # bsp = binary space partitioning. Can do bsp, stack or float
        yabai -m config layout bsp

        # new window opens in the bottom/right 
        yabai -m config window_placement second_child

        # floating windows are always on top (default: off)
        yabai -m config window_topmost off

        # sketchybar spacing
        yabai -m config external_bar all:32:0

        # padding/gaps
        yabai -m config top_padding 10
        yabai -m config bottom_padding 10
        yabai -m config left_padding 10
        yabai -m config right_padding 10
        yabai -m config window_gap 10

        # mouse
        yabai -m config mouse_follows_focus off
        yabai -m config mouse_modifier alt
        yabai -m config mouse_action1 move
        yabai -m config mouse_action2 resize
        yabai -m mouse_drop_action swap

        # app rules
        yabai -m rule --add app="^Google Chrome$" space=^1
        yabai -m rule --add app="^Safari$" space=^1
        yabai -m rule --add app="^IntelliJ IDEA$" space=^2
        yabai -m rule --add app="^WezTerm$" space=^4
        yabai -m rule --add app="^Obsidian$" space=^5
        yabai -m rule --add app="^Todoist$" space=^5
        yabai -m rule --add app="^Microsoft Teams$" space=^6
        yabai -m rule --add app="^Slack$" space=^6
        yabai -m rule --add app="^Microsoft Outlook$" space=^7
        yabai -m rule --add app="^Proton Mail$" space=^7

        # ignored apps
        yabai -m rule --add app="^System Settings$" manage=off
        yabai -m rule --add app="^Calculator$" manage=off
        yabai -m rule --add app="^Cisco AnyConnect.*$" manage=off

        # ms teams
        yabai -m signal --add event=window_created action='yabai -m query --windows --window $YABAI_WINDOW_ID | jq -e ".\"can-resize\"" || yabai -m window $YABAI_WINDOW_ID --toggle float' app="Microsoft Teams classic"
        yabai -m signal --add event=window_created action='yabai -m query --windows --window $YABAI_WINDOW_ID | jq -e ".\"can-resize\"" || yabai -m window $YABAI_WINDOW_ID --toggle float' app="Microsoft Teams (work or school)"
      '';
    };

    home.file.yabai_fix_spaces = {
      executable = true;
      target = ".config/yabai/fix_spaces.sh";
      text = ''
        #!/usr/bin/env bash

        move_app_to_space() {
            yabai -m window $(yabai -m query --windows | jq --arg app "$1" '.[] | select(.app==$app).id') --space $2
        }

        num_displays=$(yabai -m query --displays | jq length)
        num_spaces=$(yabai -m query --spaces | jq length)

        $HOME/.config/yabai/create_spaces.sh

        if [[ $num_spaces -gt 7 ]]; then
            yabai -m space --destroy 9
            yabai -m space --destroy 8
        fi

        if [[ $num_displays -eq 1 ]]; then
            yabai -m space 6 --display 1
            yabai -m space 7 --display 1
        fi

        if [[ $num_displays -eq 2 ]]; then
            yabai -m space 6 --display 2
            yabai -m space 7 --display 2
        fi

        if [[ $num_displays -eq 3 ]]; then
            yabai -m space 6 --display 2
            yabai -m space 7 --display 3
        fi

        move_app_to_space "Slack" 6
        move_app_to_space "Microsoft Teams" 6
        move_app_to_space "Microsoft Outlook" 7

        sleep 1
        sketchybar --reload
      '';
    };

    home.file.yabai_create_spaces = {
      executable = true;
      target = ".config/yabai/create_spaces.sh";
      text = ''
        #!/usr/bin/env bash

        function setup_space {
          local idx="$1"
          local name="$2"
          local space=
          echo "setup space $idx : $name"
        
          space=$(yabai -m query --spaces --space "$idx")
          if [ -z "$space" ]; then
            yabai -m space --create
          fi
        
          yabai -m space "$idx" --label "$name"
        }
        
        setup_space 1 web
        setup_space 2 code
        setup_space 3 blank
        setup_space 4 terminal
        setup_space 5 productivity
        setup_space 6 messaging
        setup_space 7 mail
        
        sketchybar --reload
      '';
    };

    home.file.skhd = {
      target = ".config/skhd/skhdrc";
      text =
        let
          yabai = "${pkgs.yabai}/bin/yabai";
        in
        ''
          # changing window focus within space
          alt - j : ${yabai} -m window --focus south
          alt - k : ${yabai} -m window --focus north
          alt - h : ${yabai} -m window --focus west
          alt - l : ${yabai} -m window --focus east

          # changing window focus between displays
          alt - s : ${yabai} -m display --focus west
          alt - g : ${yabai} -m display --focus east

          # toggle float
          shift + alt - f : ${yabai} -m window --toggle float --grid 4:4:1:1:2:2

          # toggle zoom
          shift + alt - z : ${yabai} -m window --toggle zoom-fullscreen

          # balance windows
          shift + alt - e : ${yabai} -m space --balance

          # swap windows
          shift + alt - j : ${yabai} -m window --swap south
          shift + alt - k : ${yabai} -m window --swap north
          shift + alt - h : ${yabai} -m window --swap west
          shift + alt - l : ${yabai} -m window --swap east

          # warp windows
          shift + ctrl + alt - j : ${yabai} -m window --warp south
          shift + ctrl + alt - k : ${yabai} -m window --warp north
          shift + ctrl + alt - h : ${yabai} -m window --warp west
          shift + ctrl + alt - l : ${yabai} -m window --warp east

          # toggle horizonal/vertical split
          shift + alt - x : ${yabai} -m window --toggle split

          # move windows to displays
          shift + alt - s : ${yabai} -m window --display west; ${yabai} -m display --focus west;
          shift + alt - g : ${yabai} -m window --display east; ${yabai} -m display --focus east;

          # switch to space
          alt - 1 : yabai -m space --focus 1
          alt - 2 : yabai -m space --focus 2
          alt - 3 : yabai -m space --focus 3
          alt - 4 : yabai -m space --focus 4
          alt - 5 : yabai -m space --focus 5
          alt - 6 : yabai -m space --focus 6
          alt - 7 : yabai -m space --focus 7
          alt - 8 : yabai -m space --focus 8
          alt - 9 : yabai -m space --focus 9
          alt - 0 : yabai -m space --focus 10

          # move windows to a space
          shift + alt - 1 : ${yabai} -m window --space 1;
          shift + alt - 2 : ${yabai} -m window --space 2;
          shift + alt - 3 : ${yabai} -m window --space 3;
          shift + alt - 4 : ${yabai} -m window --space 4;
          shift + alt - 5 : ${yabai} -m window --space 5;
          shift + alt - 6 : ${yabai} -m window --space 6;
          shift + alt - 7 : ${yabai} -m window --space 7;
          shift + alt - 8 : ${yabai} -m window --space 8;
          shift + alt - 9 : ${yabai} -m window --space 9;

          # operations
          # reload everything
          cmd + shift + alt + ctrl - r : ${yabai} --restart-service;
          # stop yabai 
          cmd + shift + alt + ctrl - q : ${yabai} --stop-service;
          # start yabai 
          cmd + shift + alt + ctrl - s : ${yabai} --start-service;
        '';
    };
  };
}
