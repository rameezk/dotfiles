{ lib, config, ... }:
let
  cfg = config.macos.window-management.aerospace;
in
{
  options.macos.window-management.aerospace = {
    enable = lib.mkEnableOption "enable aerospace";
  };

  config = lib.mkIf cfg.enable {
    xdg.configFile."aerospace/aerospace.toml".text = # toml
      ''
        # Place a copy of this config to ~/.aerospace.toml
        # After that, you can edit ~/.aerospace.toml to your liking

        # You can use it to add commands that run after login to macOS user session.
        # 'start-at-login' needs to be 'true' for 'after-login-command' to work
        # Available commands: https://nikitabobko.github.io/AeroSpace/commands
        after-login-command = []

        # You can use it to add commands that run after AeroSpace startup.
        # 'after-startup-command' is run after 'after-login-command'
        # Available commands : https://nikitabobko.github.io/AeroSpace/commands
        after-startup-command = []

        # Start AeroSpace at login
        start-at-login = true

        # Normalizations. See: https://nikitabobko.github.io/AeroSpace/guide#normalization
        enable-normalization-flatten-containers = true
        enable-normalization-opposite-orientation-for-nested-containers = true

        # See: https://nikitabobko.github.io/AeroSpace/guide#layouts
        # The 'accordion-padding' specifies the size of accordion padding
        # You can set 0 to disable the padding feature
        accordion-padding = 30

        # Possible values: tiles|accordion
        default-root-container-layout = 'tiles'

        # Possible values: horizontal|vertical|auto
        # 'auto' means: wide monitor (anything wider than high) gets horizontal orientation,
        #               tall monitor (anything higher than wide) gets vertical orientation
        default-root-container-orientation = 'auto'

        # Possible values: (qwerty|dvorak)
        # See https://nikitabobko.github.io/AeroSpace/guide#key-mapping
        key-mapping.preset = 'qwerty'

        # Mouse follows focus when focused monitor changes
        # Drop it from your config, if you don't like this behavior
        # See https://nikitabobko.github.io/AeroSpace/guide#on-focus-changed-callbacks
        # See https://nikitabobko.github.io/AeroSpace/commands#move-mouse
        # Fallback value (if you omit the key): on-focused-monitor-changed = []
        on-focused-monitor-changed = ['move-mouse monitor-lazy-center']
        on-focus-changed = 'move-mouse window-lazy-center'

        # Gaps between windows (inner-*) and between monitor edges (outer-*).
        # Possible values:
        # - Constant:     gaps.outer.top = 8
        # - Per monitor:  gaps.outer.top = [{ monitor.main = 16 }, { monitor."some-pattern" = 32 }, 24]
        #                 In this example, 24 is a default value when there is no match.
        #                 Monitor pattern is the same as for 'workspace-to-monitor-force-assignment'.
        #                 See: https://nikitabobko.github.io/AeroSpace/guide#assign-workspaces-to-monitors
        [gaps]
        inner.horizontal = 10
        inner.vertical =   10
        outer.left =       10
        outer.bottom =     10
        outer.top =        10
        outer.right =      10

        # 'main' binding mode declaration
        # See: https://nikitabobko.github.io/AeroSpace/guide#binding-modes
        # 'main' binding mode must be always presented
        # Fallback value (if you omit the key): mode.main.binding = {}
        [mode.main.binding]

        # All possible keys:
        # - Letters.        a, b, c, ..., z
        # - Numbers.        0, 1, 2, ..., 9
        # - Keypad numbers. keypad0, keypad1, keypad2, ..., keypad9
        # - F-keys.         f1, f2, ..., f20
        # - Special keys.   minus, equal, period, comma, slash, backslash, quote, semicolon, backtick,
        #                   leftSquareBracket, rightSquareBracket, space, enter, esc, backspace, tab
        # - Keypad special. keypadClear, keypadDecimalMark, keypadDivide, keypadEnter, keypadEqual,
        #                   keypadMinus, keypadMultiply, keypadPlus
        # - Arrows.         left, down, up, right

        # All possible modifiers: cmd, alt, ctrl, shift

        # All possible commands: https://nikitabobko.github.io/AeroSpace/commands

        # See: https://nikitabobko.github.io/AeroSpace/commands#exec-and-forget
        # You can uncomment the following lines to open up terminal with alt + enter shortcut (like in i3)
        # alt-enter = '''exec-and-forget osascript -e '
        # tell application "Terminal"
        #     do script
        #     activate
        # end tell'
        # '''

        # See: https://nikitabobko.github.io/AeroSpace/commands#layout
        ctrl-shift-alt-slash = 'layout tiles horizontal vertical'
        ctrl-shift-alt-comma = 'layout accordion horizontal vertical'

        # See: https://nikitabobko.github.io/AeroSpace/commands#focus
        ctrl-shift-alt-h = 'focus left'
        ctrl-shift-alt-j = 'focus down'
        ctrl-shift-alt-k = 'focus up'
        ctrl-shift-alt-l = 'focus right'

        # See: https://nikitabobko.github.io/AeroSpace/commands#move
        ctrl-shift-alt-cmd-h = 'move left'
        ctrl-shift-alt-cmd-j = 'move down'
        ctrl-shift-alt-cmd-k = 'move up'
        ctrl-shift-alt-cmd-l = 'move right'

        # See: https://nikitabobko.github.io/AeroSpace/commands#resize
        ctrl-alt-shift-minus = 'resize smart -50'
        ctrl-alt-shift-equal = 'resize smart +50'

        # See: https://nikitabobko.github.io/AeroSpace/commands#workspace
        ctrl-shift-alt-1 = 'workspace 1'
        ctrl-shift-alt-2 = 'workspace 2'
        ctrl-shift-alt-3 = 'workspace 3'
        ctrl-shift-alt-4 = 'workspace 4'
        ctrl-shift-alt-5 = 'workspace 5'
        ctrl-shift-alt-6 = 'workspace 6'
        ctrl-shift-alt-7 = 'workspace 7'
        ctrl-shift-alt-8 = 'workspace 8'
        ctrl-shift-alt-9 = 'workspace 9'
        ctrl-shift-alt-a = 'workspace A' # In your config, you can drop workspace bindings that you don't need
        ctrl-shift-alt-b = 'workspace B'
        ctrl-shift-alt-c = 'workspace C'
        ctrl-shift-alt-d = 'workspace D'
        ctrl-shift-alt-e = 'workspace E'
        ctrl-shift-alt-g = 'workspace G'
        ctrl-shift-alt-i = 'workspace I'
        ctrl-shift-alt-m = 'workspace M'
        ctrl-shift-alt-n = 'workspace N'
        ctrl-shift-alt-o = 'workspace O'
        ctrl-shift-alt-p = 'workspace P'
        ctrl-shift-alt-q = 'workspace Q'
        ctrl-shift-alt-r = 'workspace R'
        ctrl-shift-alt-s = 'workspace S'
        ctrl-shift-alt-t = 'workspace T'
        ctrl-shift-alt-u = 'workspace U'
        ctrl-shift-alt-v = 'workspace V'
        ctrl-shift-alt-w = 'workspace W'
        ctrl-shift-alt-x = 'workspace X'
        ctrl-shift-alt-y = 'workspace Y'
        ctrl-shift-alt-z = 'workspace Z'

        # fullscreen window
        ctrl-shift-alt-f = 'fullscreen'

        # See: https://nikitabobko.github.io/AeroSpace/commands#move-node-to-workspace
        ctrl-shift-alt-cmd-1 = 'move-node-to-workspace 1'
        ctrl-shift-alt-cmd-2 = 'move-node-to-workspace 2'
        ctrl-shift-alt-cmd-3 = 'move-node-to-workspace 3'
        ctrl-shift-alt-cmd-4 = 'move-node-to-workspace 4'
        ctrl-shift-alt-cmd-5 = 'move-node-to-workspace 5'
        ctrl-shift-alt-cmd-6 = 'move-node-to-workspace 6'
        ctrl-shift-alt-cmd-7 = 'move-node-to-workspace 7'
        ctrl-shift-alt-cmd-8 = 'move-node-to-workspace 8'
        ctrl-shift-alt-cmd-9 = 'move-node-to-workspace 9'
        ctrl-shift-alt-cmd-a = 'move-node-to-workspace A'
        ctrl-shift-alt-cmd-b = 'move-node-to-workspace B'
        ctrl-shift-alt-cmd-c = 'move-node-to-workspace C'
        ctrl-shift-alt-cmd-d = 'move-node-to-workspace D'
        ctrl-shift-alt-cmd-e = 'move-node-to-workspace E'
        ctrl-shift-alt-cmd-g = 'move-node-to-workspace G'
        ctrl-shift-alt-cmd-i = 'move-node-to-workspace I'
        ctrl-shift-alt-cmd-m = 'move-node-to-workspace M'
        ctrl-shift-alt-cmd-n = 'move-node-to-workspace N'
        ctrl-shift-alt-cmd-o = 'move-node-to-workspace O'
        ctrl-shift-alt-cmd-p = 'move-node-to-workspace P'
        ctrl-shift-alt-cmd-q = 'move-node-to-workspace Q'
        ctrl-shift-alt-cmd-r = 'move-node-to-workspace R'
        ctrl-shift-alt-cmd-s = 'move-node-to-workspace S'
        ctrl-shift-alt-cmd-t = 'move-node-to-workspace T'
        ctrl-shift-alt-cmd-u = 'move-node-to-workspace U'
        ctrl-shift-alt-cmd-v = 'move-node-to-workspace V'
        ctrl-shift-alt-cmd-w = 'move-node-to-workspace W'
        ctrl-shift-alt-cmd-x = 'move-node-to-workspace X'
        ctrl-shift-alt-cmd-y = 'move-node-to-workspace Y'
        ctrl-shift-alt-cmd-z = 'move-node-to-workspace Z'

        # See: https://nikitabobko.github.io/AeroSpace/commands#workspace-back-and-forth
        alt-tab = 'workspace-back-and-forth'
        # See: https://nikitabobko.github.io/AeroSpace/commands#move-workspace-to-monitor
        alt-shift-tab = 'move-workspace-to-monitor --wrap-around next'

        # See: https://nikitabobko.github.io/AeroSpace/commands#mode
        ctrl-alt-shift-semicolon = 'mode service'

        # 'service' binding mode declaration.
        # See: https://nikitabobko.github.io/AeroSpace/guide#binding-modes
        [mode.service.binding]
        esc = ['reload-config', 'mode main']
        r = ['flatten-workspace-tree', 'mode main'] # reset layout
        f = ['layout floating tiling', 'mode main'] # Toggle between floating and tiling layout
        backspace = ['close-all-windows-but-current', 'mode main']

        # sticky is not yet supported https://github.com/nikitabobko/AeroSpace/issues/2
        #s = ['layout sticky tiling', 'mode main']

        ctrl-alt-shift-h = ['join-with left', 'mode main']
        ctrl-alt-shift-j = ['join-with down', 'mode main']
        ctrl-alt-shift-k = ['join-with up', 'mode main']
        ctrl-alt-shift-l = ['join-with right', 'mode main']
      '';
  };
}
