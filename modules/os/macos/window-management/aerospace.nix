{
  lib,
  config,
  pkgs,
  ...
}:
let
  cfg = config.macos.window-management.aerospace;

  helium-profile = pkgs.writeShellScriptBin "helium-profile" ''
    name="$1"
    aerospace="/opt/homebrew/bin/aerospace"

    win=$("$aerospace" list-windows --all --format '%{window-id}|%{window-title}' \
      | ${pkgs.gnugrep}/bin/grep -F "Helium – $name" \
      | head -n1 | cut -d'|' -f1 | tr -d '[:space:]')
    if [ -n "$win" ]; then
      exec "$aerospace" focus --window-id "$win"
    fi

    state="$HOME/Library/Application Support/net.imput.helium/Local State"
    dir=$(${pkgs.jq}/bin/jq -r --arg n "$name" \
      '.profile.info_cache | to_entries[] | select(.value.name == $n) | .key' \
      "$state" | head -n1)
    if [ -z "$dir" ]; then
      dir="$name"
    fi
    exec "/Applications/Helium.app/Contents/MacOS/Helium" --profile-directory="$dir"
  '';
in
{
  options.macos.window-management.aerospace = {
    enable = lib.mkEnableOption "enable aerospace";
  };

  config = lib.mkIf cfg.enable {
    xdg.configFile."aerospace/aerospace.toml".text = # toml
      ''
        config-version = 2

        after-login-command = []
        after-startup-command = []

        start-at-login = true

        persistent-workspaces = ['1', '2', '3', '4', '5']

        enable-normalization-flatten-containers = true
        enable-normalization-opposite-orientation-for-nested-containers = true

        automatically-unhide-macos-hidden-apps = true

        accordion-padding = 30

        default-root-container-layout = 'tiles'
        default-root-container-orientation = 'auto'

        key-mapping.preset = 'qwerty'

        on-focused-monitor-changed = ['move-mouse monitor-lazy-center']
        on-focus-changed = 'move-mouse window-lazy-center'

        [gaps]
        inner.horizontal = 10
        inner.vertical =   10
        outer.left =       10
        outer.bottom =     10
        outer.top =        10
        outer.right =      10

        [workspace-to-monitor-force-assignment]
        5 = 'built-in'

        [mode.main.binding]
        ctrl-alt-cmd-h = 'focus left'
        ctrl-alt-cmd-j = 'focus down'
        ctrl-alt-cmd-k = 'focus up'
        ctrl-alt-cmd-l = 'focus right'

        ctrl-alt-cmd-shift-h = 'move left'
        ctrl-alt-cmd-shift-j = 'move down'
        ctrl-alt-cmd-shift-k = 'move up'
        ctrl-alt-cmd-shift-l = 'move right'

        ctrl-alt-cmd-1 = 'workspace 1'
        ctrl-alt-cmd-2 = 'workspace 2'
        ctrl-alt-cmd-3 = 'workspace 3'
        ctrl-alt-cmd-4 = 'workspace 4'
        ctrl-alt-cmd-5 = 'workspace 5'

        ctrl-alt-cmd-shift-1 = 'move-node-to-workspace 1'
        ctrl-alt-cmd-shift-2 = 'move-node-to-workspace 2'
        ctrl-alt-cmd-shift-3 = 'move-node-to-workspace 3'
        ctrl-alt-cmd-shift-4 = 'move-node-to-workspace 4'
        ctrl-alt-cmd-shift-5 = 'move-node-to-workspace 5'

        ctrl-alt-cmd-g = 'workspace-back-and-forth'
        ctrl-alt-cmd-tab = 'move-workspace-to-monitor --wrap-around next'
        ctrl-alt-cmd-f = 'fullscreen'
        ctrl-alt-cmd-w = 'layout tiles accordion'
        ctrl-alt-cmd-p = 'layout floating tiling'
        ctrl-alt-cmd-b = 'flatten-workspace-tree'
        ctrl-alt-cmd-minus = 'resize smart -50'
        ctrl-alt-cmd-equal = 'resize smart +50'
        ctrl-alt-cmd-0 = 'balance-sizes'
        ctrl-alt-cmd-s = 'layout tiles horizontal vertical'
        ctrl-alt-cmd-semicolon = 'mode manage'
        ctrl-alt-cmd-space = 'mode launch'
        ctrl-alt-cmd-shift-f13 = 'exec-and-forget /usr/bin/shortcuts run "Toggle Key Lights"'
        ctrl-alt-cmd-shift-f14 = 'exec-and-forget open -a ScreenSaverEngine'

        [mode.manage.binding]
        ctrl-alt-cmd-h = ['join-with left', 'mode main']
        ctrl-alt-cmd-j = ['join-with down', 'mode main']
        ctrl-alt-cmd-k = ['join-with up', 'mode main']
        ctrl-alt-cmd-l = ['join-with right', 'mode main']
        r = ['reload-config', 'mode main']
        esc = 'mode main'

        [mode.launch.binding]
        a = ['exec-and-forget open -a Claude', 'mode main']
        b = 'mode browser'
        c = ['exec-and-forget open -a "Microsoft Teams"', 'mode main']
        e = ['exec-and-forget open -a "IntelliJ IDEA"', 'mode main']
        l = ['exec-and-forget open -a Todoist', 'mode main']
        n = ['exec-and-forget open -a Obsidian', 'mode main']
        o = ['exec-and-forget open -a "Microsoft Outlook"', 'mode main']
        s = ['exec-and-forget open -a Slack', 'mode main']
        t = ['exec-and-forget open -a WezTerm', 'mode main']
        esc = 'mode main'

        [mode.browser.binding]
        b = ['exec-and-forget ${helium-profile}/bin/helium-profile "M&S"', 'mode main']
        e = ['exec-and-forget ${helium-profile}/bin/helium-profile EE', 'mode main']
        p = ['exec-and-forget ${helium-profile}/bin/helium-profile Personal', 'mode main']
        esc = 'mode main'

        [[on-window-detected]]
        if.app-id = 'com.apple.systempreferences'
        run = 'layout floating'

        [[on-window-detected]]
        if.app-id = 'com.1password.1password'
        run = 'layout floating'

        [[on-window-detected]]
        if.app-id = 'org.pqrs.Karabiner-Elements.Settings'
        run = 'layout floating'

        [[on-window-detected]]
        if.app-id = 'com.apple.calculator'
        run = 'layout floating'

        [[on-window-detected]]
        if.app-id = 'com.apple.ActivityMonitor'
        run = 'layout floating'
      '';
  };
}
