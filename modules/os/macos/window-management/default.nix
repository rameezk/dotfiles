{ pkgs, ... }: {

  home.packages = with pkgs;
    [
      yabai # i3wm alternative
    ];

  home.file.yabai = { executable = true; };

}
