{ pkgs, ... }: {

  home.packages = with pkgs; [
    skhd # keyboard hotkey daemon
    yabai # i3wm alternative
  ];

  home.file.yabai = {
    executable = true;
    target = ".config/yabai/yabairc";
    text = ''
      # layout
      # bsp = binary space partitioning. Can do bsp, stack or float
      yabai -m config layout bsp

      # new window opens in the bottom/right 
      yabai -m config window_placement second_child

      # padding/gaps
      yabai -m config top_padding 12
      yabai -m config bottom_padding 12
      yabai -m config left_padding 12
      yabai -m config right_padding 12
      yabai -m config window_gap 12

      # mouse
      yabai -m config mouse_follows_focus off
      yabai -m config mouse_modifier alt
      yabai -m config mouse_action1 move
      yabai -m config mouse_action2 resize
      yabai -m mouse_drop_action swap

      # managed apps
      yabai -m rule --add app=Emacs manage=on space=9

      # ignored apps
      yabai -m rule --add app="^System Settings$" manage=off
      yabai -m rule --add app="^Calculator$" manage=off
      yabai -m rule --add app="^Cisco AnyConnect.*$" manage=off

      # ms teams
      yabai -m signal --add event=window_created action='yabai -m query --windows --window $YABAI_WINDOW_ID | jq -e ".\"can-resize\"" || yabai -m window $YABAI_WINDOW_ID --toggle float' app="Microsoft Teams classic"
      yabai -m signal --add event=window_created action='yabai -m query --windows --window $YABAI_WINDOW_ID | jq -e ".\"can-resize\"" || yabai -m window $YABAI_WINDOW_ID --toggle float' app="Microsoft Teams (work or school)"
    '';
  };

  home.file.skhd = {
    target = ".config/skhd/skhdrc";
    text = let yabai = "${pkgs.yabai}/bin/yabai";
    in ''
      # changing window focus within space
      alt - j : ${yabai} -m window --focus south
      alt - k : ${yabai} -m window --focus north
      alt - h : ${yabai} -m window --focus west
      alt - l : ${yabai} -m window --focus east

      # changing window focus between displays
      alt - s : ${yabai} -m display --focus west
      alt - g : ${yabai} -m display --focus east

      # toggle float
      shift + alt - t : ${yabai} -m window --toggle float --grid 4:4:1:1:2:2

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
      cmd + shift + alt + ctrl - r : ${yabai} --restart-service; launchctl unload /Library/LaunchAgents/org.nixos.skhd.plist; launchctl load -w /Library/LaunchAgents/org.nixos.skhd.plist;
      # stop yabai 
      cmd + shift + alt + ctrl - q : ${yabai} --stop-service;
      # start yabai 
      cmd + shift + alt + ctrl - s : ${yabai} --start-service;
    '';
  };

}
