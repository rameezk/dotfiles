# AeroSpace Migration Plan

Moving from Moom to AeroSpace, keyed for a ZSA Moonlander (Colemak-DH) at the
desk and the MacBook's built-in QWERTY keyboard on the road.

## The model

One leader modifier, one tier switch. That's the whole system.

- **Leader = `Super` (Ctrl + Opt + Cmd)** - hold it to talk to AeroSpace.
- **Add `Shift`** to turn *focus/go* into *move/send*.
- Directions are always `h j k l` (left / down / up / right); workspaces are digits.

One rule to remember:

> Hold leader → `hjkl` moves your **focus**. Add **Shift** → it drags the **window**. Digits are **workspaces**.

## Leaders (how each keyboard produces `Super`)

| Keyboard | Leader | Mechanism |
| --- | --- | --- |
| Built-in (QWERTY) | **Caps Lock** | Hold = `Super`. Tap = `Esc` (unchanged). No layer - `hjkl` is already home row. |
| Moonlander (Colemak) | **Left `Hyper` thumb** | Hold = enter the WM layer. Home-row keys emit `Super + <key>`. Add `Shift` for move. |

Raw `Hyper` is still available if ever needed: Moonlander left-pinky `Esc`/`Hyper`
key, or `Caps + Shift` on the built-in.

## Cheat sheet

`Caps` below = the leader (`Super`). On the Moonlander, hold the left thumb WM-layer
key instead; the keys land in the same spots.

| Action | Keys | AeroSpace command |
| --- | --- | --- |
| Focus left / down / up / right | `Caps + h / j / k / l` | `focus left/down/up/right` |
| Move window left / down / up / right | `Caps + Shift + h / j / k / l` | `move left/down/up/right` |
| Go to workspace 1-5 | `Caps + 1 … 5` | `workspace 1…5` |
| Send window to workspace 1-5 | `Caps + Shift + 1 … 5` | `move-node-to-workspace 1…5` |
| Previous workspace (toggle) | `Caps + g` | `workspace-back-and-forth` |
| Bounce workspace to other monitor | `Caps + Tab` | `move-workspace-to-monitor --wrap-around next` |
| Fullscreen (toggle) | `Caps + f` | `fullscreen` |
| Cycle layout (tiles / accordion) | `Caps + w` | `layout tiles accordion` |
| Flip split orientation (horizontal / vertical) | `Caps + s` | `layout tiles horizontal vertical` |
| Float / tile focused window | `Caps + p` | `layout floating tiling` |
| Reset / flatten workspace tree | `Caps + b` | `flatten-workspace-tree` |
| Enter manage mode (to group windows) | `Caps + ;` | `mode manage` |
| Join window with neighbour (in manage mode, auto-exits) | `Caps + h / j / k / l` | `join-with left/down/up/right` |
| Enter launch mode (then an app/util key) | `Caps + o` | `mode launch` |
| Shrink / grow focused window | `Caps + - / =` | `resize smart -50 / +50` |
| Reload config | `Caps + r` | `reload-config` |

## Launch mode (retired Alfred Hyper launchers)

`Caps + o` enters launch mode; the next key opens the target and returns to main.
Replaces the old Alfred `Hyper + <key>` launchers (folded in here so app-launching
reuses the leader instead of the Hyper namespace the move tier now owns).

Entry works per keyboard: built-in uses `Caps + o`; the Moonlander uses a key emitting
`Hyper + F14` (`ctrl-alt-cmd-shift-f14`, which it already has). Once in, the app key is a
bare letter - tap it on the normal layer and it opens + returns to main. Manage mode is
the same shape: `Caps + ;` or `Hyper + F15` to enter, then `h/j/k/l` (positional, so on
the Moonlander hold the WM layer and use `N/E/I/O`).

| Key | Opens | Command |
| --- | --- | --- |
| `a` | Claude | `open -a Claude` |
| `b` | Google Chrome | `open -a "Google Chrome"` |
| `c` | Microsoft Teams | `open -a "Microsoft Teams"` |
| `e` | IntelliJ IDEA | `open -a "IntelliJ IDEA"` |
| `l` | Todoist | `open -a Todoist` |
| `m` | Safari | `open -a Safari` |
| `n` | Obsidian | `open -a Obsidian` |
| `o` | Microsoft Outlook | `open -a "Microsoft Outlook"` |
| `s` | Slack | `open -a Slack` |
| `t` | WezTerm | `open -a WezTerm` |
| `k` | Key Lights (toggle) | `shortcuts run "Toggle Key Lights"` |
| `v` | Screen Saver | `open -a ScreenSaverEngine` |

Key letters match the old Alfred bindings; the two former function-key launchers get
new mnemonics: `k` (Key Lights, was `F13`) and `v` (screensaVer, was `F14`).

## Moonlander WM layer (physical placement)

Hold the left `Hyper` thumb to reach this layer. Right hand = directions, left hand
= workspaces, left-top = toggles. Add `Shift` (right thumb or home-row-mod on `T`/`N`)
for the move/send tier.

```
        LEFT HAND                              RIGHT HAND
 top    W    F    P    B                       (directions on home row)
 home   A    R    S    T    G                  N     E     I     O
        1    2    3    4    5                  h     j     k     l
                                               left  down  up    right

 misc   Tab = bounce to monitor   g = prev ws   r = reload   - / = resize
        s = flip split (H/V)       ; = manage mode (then join with N/E/I/O)
```

Keycode mapping in the layer: `N E I O` send `h j k l`; `A R S T G` send `1 2 3 4 5`;
`W F P B` send `w f p b`. Base layout (Colemak) is irrelevant - the layer emits fixed
keycodes, so it matches the built-in QWERTY bindings exactly.

The WM layer only carries the **positional** core (`Super + h j k l`, `Super + 1-5`,
and the toggles `w f p b s g Tab r - =`). Manage-mode join-with reuses `N E I O`
(= `h j k l`), so it needs no new keys - hold the layer, tap `;` (`Super + ;` → manage),
keep holding, tap `N/E/I/O`.

Launch mode is handled differently, because it is **letter-mnemonic, not positional**
(`s` = Slack by name). So it does *not* live in the WM layer at all:

- **Entry** reuses the reserved F-key convention - a single key emitting `Hyper + F14`
  enters launch mode; `Hyper + F15` enters manage mode. Your firmware already has a
  `Hyper + F14` key (layer 3). AeroSpace binds both `ctrl-alt-cmd-shift-f14/f15` and the
  `Caps + o` / `Caps + ;` forms, so the F-keys serve the Moonlander and `Caps` serves
  the built-in.
- **App keys** are typed on the **normal base (Colemak) layer** after entry - launch
  mode has bare-letter bindings, so pressing the key that types `s` opens Slack. No
  launch letters in the WM layer.

Step 4 firmware work, net: convert the left `Hyper` thumb to `MO(WM)`; build the WM
layer's positional core above; point the existing `Hyper + F14` key (and add a
`Hyper + F15` key) at launch/manage entry. Nothing else to mirror.

## AeroSpace bindings (implementation reference)

```toml
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
ctrl-alt-cmd-s = 'layout tiles horizontal vertical'
ctrl-alt-cmd-semicolon = 'mode manage'
ctrl-alt-cmd-o = 'mode launch'
ctrl-alt-cmd-shift-f14 = 'mode launch'
ctrl-alt-cmd-shift-f15 = 'mode manage'
ctrl-alt-cmd-minus = 'resize smart -50'
ctrl-alt-cmd-equal = 'resize smart +50'
ctrl-alt-cmd-r = 'reload-config'

[mode.manage.binding]
h = ['join-with left', 'mode main']
j = ['join-with down', 'mode main']
k = ['join-with up', 'mode main']
l = ['join-with right', 'mode main']
ctrl-alt-cmd-h = ['join-with left', 'mode main']
ctrl-alt-cmd-j = ['join-with down', 'mode main']
ctrl-alt-cmd-k = ['join-with up', 'mode main']
ctrl-alt-cmd-l = ['join-with right', 'mode main']
esc = 'mode main'

[mode.launch.binding]
a = ['exec-and-forget open -a Claude', 'mode main']
b = ['exec-and-forget open -a "Google Chrome"', 'mode main']
c = ['exec-and-forget open -a "Microsoft Teams"', 'mode main']
e = ['exec-and-forget open -a "IntelliJ IDEA"', 'mode main']
l = ['exec-and-forget open -a Todoist', 'mode main']
m = ['exec-and-forget open -a Safari', 'mode main']
n = ['exec-and-forget open -a Obsidian', 'mode main']
o = ['exec-and-forget open -a "Microsoft Outlook"', 'mode main']
s = ['exec-and-forget open -a Slack', 'mode main']
t = ['exec-and-forget open -a WezTerm', 'mode main']
k = ['exec-and-forget /usr/bin/shortcuts run "Toggle Key Lights"', 'mode main']
v = ['exec-and-forget open -a ScreenSaverEngine', 'mode main']
ctrl-alt-cmd-a = ['exec-and-forget open -a Claude', 'mode main']
ctrl-alt-cmd-b = ['exec-and-forget open -a "Google Chrome"', 'mode main']
ctrl-alt-cmd-c = ['exec-and-forget open -a "Microsoft Teams"', 'mode main']
ctrl-alt-cmd-e = ['exec-and-forget open -a "IntelliJ IDEA"', 'mode main']
ctrl-alt-cmd-l = ['exec-and-forget open -a Todoist', 'mode main']
ctrl-alt-cmd-m = ['exec-and-forget open -a Safari', 'mode main']
ctrl-alt-cmd-n = ['exec-and-forget open -a Obsidian', 'mode main']
ctrl-alt-cmd-o = ['exec-and-forget open -a "Microsoft Outlook"', 'mode main']
ctrl-alt-cmd-s = ['exec-and-forget open -a Slack', 'mode main']
ctrl-alt-cmd-t = ['exec-and-forget open -a WezTerm', 'mode main']
ctrl-alt-cmd-k = ['exec-and-forget /usr/bin/shortcuts run "Toggle Key Lights"', 'mode main']
ctrl-alt-cmd-v = ['exec-and-forget open -a ScreenSaverEngine', 'mode main']
esc = 'mode main'
```

## Displays

- **Ultrawide** (main): `tiles`, kept to 2-3 columns. Spread extra windows across
  workspaces rather than cramming one.
- **MacBook** (physically left of the ultrawide): home for one or two pinned
  workspaces (comms / music). `focus left` off the ultrawide's edge walks onto it,
  matching the physical geometry.
- Pin via `workspace-to-monitor-force-assignment` (workspaces TBD, see open items).

## Migration steps

Do these in order. The built-in path (steps 1-2) needs **no firmware work** and
drives the whole scheme, so validate it and live on it before building the
Moonlander layer. The firmware layer (step 4) just replicates a scheme you have
already confirmed. Every step is reversible.

### Step 1 - Karabiner (built-in leader) [DO FIRST]

Edit `modules/os/macos/karabiner/config/karabiner.json` (managed by the
`macos.karabiner` module, out-of-store-symlinked to `~/.config/karabiner/karabiner.json`
so edits are live and version-controlled). The profile has a
single complex-modification rule on `caps_lock`. Change the held output from four
mods to three - drop `left_shift`. Keep the tap = Escape behaviour.

Current manipulator:

```json
{
  "from": { "key_code": "caps_lock", "modifiers": { "optional": ["any"] } },
  "to": [{ "key_code": "left_shift",
           "modifiers": ["left_command", "left_control", "left_option"] }],
  "to_if_alone": [{ "key_code": "escape" }]
}
```

Target manipulator (held = `Super`, not Hyper):

```json
{
  "from": { "key_code": "caps_lock", "modifiers": { "optional": ["any"] } },
  "to": [{ "key_code": "left_command",
           "modifiers": ["left_control", "left_option"] }],
  "to_if_alone": [{ "key_code": "escape" }]
}
```

Karabiner-Elements is installed via `machines/fangorn/casks.nix`, and its config is
now managed by the `macos.karabiner` module (enabled on `fangorn`). Follow-up: enable
`macos.karabiner.enable` on `rivendell` too if that machine should share the config.

### Step 2 - `aerospace.nix` [DO SECOND]

File: `modules/os/macos/window-management/aerospace.nix`. Enabled on `rivendell`
(`machines/rivendell/home.nix:60`). It currently holds the **stock** upstream config
(full of upstream comments). Replace the `[mode.main.binding]` block with the
bindings under "AeroSpace bindings" above (no comments - repo rule). Also:

- `default-root-container-layout = 'tiles'`, ultrawide kept to 2-3 columns.
- Add `[workspace-to-monitor-force-assignment]` pinning the MacBook workspace(s).
- Add `[[on-window-detected]]` float rules (System Settings, 1Password, etc.).

Apply and reload:

```bash
./bin/switch
aerospace reload-config
```

Now test the entire scheme on the built-in keyboard. Tune workspace count and
monitor pinning here before touching firmware.

### Step 3 - Retire Alfred Hyper launchers [DONE]

Under the new scheme `Super + Shift` equals Hyper, so the move tier reuses the
Hyper namespace these workflows owned. Both were folded into AeroSpace **launch
mode** (see the "Launch mode" section above) and then **disabled** rather than
deleted - `disabled = true` in each `info.plist`, so they stay linked but inactive
and reversible while the migration settles. Workflows live in
`modules/os/macos/alfred/`:

- `launcher` - was `Hyper + A S T B C E L M N O` + `F14` -> launch mode `a … t`, `v`
- `Key Lights` - was `Hyper + F13` -> launch mode `k`

Freeing `Hyper + F13/F14` is what lets the Moonlander reuse `Hyper + F14/F15` as the
launch/manage mode-entry keys. Delete the disabled workflows for good once the whole
scheme (including firmware) feels right.

The two Moonlander firmware macros that *type into Alfred* (`ST_MACRO_0` = sleep,
`ST_MACRO_1` = `bm `) are not Hyper hotkeys and keep working.

### Step 4 - Moonlander firmware (Oryx) [DO LAST]

Only after the built-in scheme feels right. Source of truth is Oryx, not this repo.

- Oryx layout hashes (from filenames): `BqRKB` / `B4ZlRB`. Open at
  `https://configure.zsa.io/moonlander/layouts/BqRKB`.
- Downloaded source zip: repo root `zsa_moonlander_reva_BqRKB_B4ZlRB_augmentatio_source.zip`
  (contains `keymap.c`). Base layer is **Colemak-DH** with home-row mods on `A R S T`
  / `N E I O`. The current left thumb is `KC_HYPR` (the key to repurpose).
- The compiled `.bin` is downloaded from Oryx per-build; keep it out of the repo.

Changes in Oryx:

1. Convert the left `Hyper` thumb from `KC_HYPR` to a momentary WM layer (`MO(n)`).
2. Build that layer per the "Moonlander WM layer" map above - each key emits
   `Super + <key>` (`Cmd+Ctrl+Opt+<key>`). Move tier = the physical `Shift`.
3. Compile, download the new `.bin`, flash via Keymapp.

Raw Hyper remains on the left-pinky `Esc`/`Hyper` key (`ALL_T(KC_ESCAPE)`).

### Step 5 - Verify on both keyboards

Walk the cheat sheet on the Moonlander and the built-in. Confirm identical behaviour
(same `hjkl`, same digits, Shift = move on both).

## Open items (still to decide)

- **Workspace count / naming** - 5 numbered as drafted, or per-app named workspaces.
- **MacBook pinning** - which workspace(s) live permanently on the built-in display.
- **App launchers** - fold the retired Alfred launchers into AeroSpace on the leader
  (`ctrl-alt-cmd-<letter> = exec-and-forget open -a <App>`), or drop them entirely.

## Decisions locked (context for a fresh session)

- Leader = `Super` (Ctrl+Opt+Cmd). Move/send tier = add `Shift`. Same on both boards.
- Built-in leader = Caps Lock (hold=Super, tap=Esc) via Karabiner.
- Moonlander leader = left thumb, repurposed from `KC_HYPR` to a momentary WM layer;
  layer keys emit `Super + <key>`; move = physical Shift.
- Directions = `hjkl` (left/down/up/right); workspaces = digits. Position-based, so
  Colemak vs QWERTY is irrelevant.
- Ultrawide = tiles, 2-3 columns. MacBook (physically left) hosts pinned workspaces.
- Alfred Hyper launchers are being retired (user accepted the loss).
- Sequencing: built-in path first (reversible, no flashing); firmware last.
```
