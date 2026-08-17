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
| Enter launch mode (then an app key) | `Caps + Space` | `mode launch` |
| Shrink / grow focused window | `Caps + - / =` | `resize smart -50 / +50` |
| Reload config | `Caps + r` | `reload-config` |

## Launch mode (retired Alfred Hyper launchers)

`Caps + Space` enters launch mode; the next key opens the target and returns to main.
Replaces the old Alfred `Hyper + <key>` launchers (folded in here so app-launching
reuses the leader instead of the Hyper namespace the move tier now owns).

Entry is a leader chord on both keyboards, so it's identical: built-in `Caps + Space`;
Moonlander = WM-layer `Space` (emits `Super + Space`). Once in, the app key is a bare
letter - tap it on the normal layer and it opens + returns to main. Manage mode is the
same shape via `Super + ;` (built-in `Caps + ;`; Moonlander WM-layer `G`), then
`h/j/k/l` (positional, so on the Moonlander hold the WM layer and use the direction
keys). No dedicated F-key mode entries needed.

Launch mode holds **apps only**; the two utilities are one-tap dedicated keys instead
(see below).

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

Key letters match the old Alfred bindings.

### One-tap utility keys (not in a mode)

Key Lights and Screen Saver are single dedicated `Hyper + F-key` presses, mapped by
AeroSpace to a direct `exec-and-forget`. Their old Moonlander keys already emit these
chords, so no firmware change is needed for them:

| Chord | Action | Command |
| --- | --- | --- |
| `Hyper + F13` | Key Lights (toggle) | `shortcuts run "Toggle Key Lights"` |
| `Hyper + F14` | Screen Saver | `open -a ScreenSaverEngine` |

These are Moonlander-only (the built-in has no F13/F14) - which is fine, since Key Lights
is desk-only and Screen Saver has native macOS alternatives on the road.

## Moonlander WM layer (physical placement)

Hold the left `Hyper` thumb (now `MO(WM)`) to reach this layer. **Hands are swapped
vs the built-in** to match existing Moonlander muscle memory: **left hand = directions**
(in the inverted-T arrow shape), **right hand = workspaces**, left-hand spares = toggles.
Shift for the move/send tier stays on the right thumb.

This diverges from the built-in only in *which hand* (built-in keeps `hjkl` on the right
home row, since QWERTY has no good left-hand `hjkl`). The keycodes, cheat sheet, and
mental model are identical; only the physical hand differs, which is fine because the
Moonlander is the primary board and the built-in is the on-the-road fallback.

```
        LEFT HAND (directions, inverted-T)     RIGHT HAND (workspaces)
 top    Q=f  W=w  [K↑ on F]  P=p  B=b          (digits on home row)
 home   A=s  R=h  S=j        T=l  G=;          M    N    E    I    O
              left down       right             1    2    3    4    5
 bottom Z=g  X=Tab C=r  D=-  V=(=)
 thumbs Space = Super+Space (launch mode)     right thumb Shift = move/send tier

   Directions: R=h(left) S=j(down) F=k(up, above S) T=l(right)
```

Keycode mapping in the layer (all emitted as `Super + <key>`): directions `R S F T`
send `h j k l` (inverted-T); workspaces `M N E I O` send `1 2 3 4 5`. Base layout
(Colemak) is irrelevant - the layer emits fixed keycodes, so the Mac side is identical
to the built-in bindings.

The WM layer carries the **positional** core (`Super + h j k l` on the left,
`Super + 1-5` on the right) plus the toggles `w f p b s g Tab r - =` on the left-hand
spare keys. Manage-mode join-with reuses the direction keys - hold the layer, tap `G`
(`Super + ;` → manage), keep holding, tap `R/S/F/T`.

Launch mode is handled differently, because it is **letter-mnemonic, not positional**
(`s` = Slack by name). So it does *not* live in the WM layer at all:

- **Entry** is a leader chord in the WM layer: `Space` -> `Super + Space` (launch),
  `G` -> `Super + ;` (manage). Same chords as the built-in (`Caps + Space` / `Caps + ;`),
  so no dedicated F-keys are needed for mode entry.
- **App keys** are typed on the **normal base (Colemak) layer** after entry - launch
  mode has bare-letter bindings, so pressing the key that types `s` opens Slack. No
  launch letters in the WM layer.

Step 4 firmware work, net: convert the left `Hyper` thumb to `MO(WM)`; build the WM
layer's positional core above (including `Space` -> `Super + Space` for launch and
`G` -> `Super + ;` for manage); keep the existing `Hyper + F13/F14` keys (Key Lights /
Screen Saver, one-tap). Nothing else to mirror.

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
ctrl-alt-cmd-space = 'mode launch'
ctrl-alt-cmd-shift-f13 = 'exec-and-forget /usr/bin/shortcuts run "Toggle Key Lights"'
ctrl-alt-cmd-shift-f14 = 'exec-and-forget open -a ScreenSaverEngine'
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

- `launcher` - was `Hyper + A S T B C E L M N O` -> launch mode `a … t`; the `F14`
  Screen Saver command is now a one-tap AeroSpace `exec` on that same `Hyper + F14` key.
- `Key Lights` - was `Hyper + F13` -> one-tap AeroSpace `exec` on the same `Hyper + F13`
  key.

The `Hyper + F13/F14` keys keep their original one-tap meaning (Key Lights / Screen
Saver) via AeroSpace `exec` bindings; launch/manage modes are entered by leader chords
(`Super + Space` / `Super + ;`), not F-keys. Delete the disabled workflows for good once
the whole scheme (including firmware) feels right.

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

In Oryx, every WM-layer key gets `Super` = **Left Ctrl + Left Alt + Left GUI** added as
modifiers to its base keycode (e.g. `H` -> `LCTL(LALT(LGUI(KC_H)))`).

1. Add a new layer (WM). Convert the left `Hyper` thumb from `KC_HYPR` to `MO(WM)`.
2. **Left hand - directions** (inverted-T, matches the existing arrow cluster), all
   `+Super`: `R`->`H` (left), `S`->`J` (down), `F`->`K` (up, above S), `T`->`L` (right).
3. **Right hand - workspaces** (contiguous, left->right = 1..5), all `+Super`:
   `M`->`1`, `N`->`2`, `E`->`3`, `I`->`4`, `O`->`5`.
4. **Move/send tier:** leave the right-thumb `Shift` transparent in the WM layer. Move =
   WM (left thumb) + Shift (right thumb) + direction; send = WM + Shift + digit.
5. **Toggles/extras** on the left-hand spares, all `+Super`: `Q`->`F` (fullscreen),
   `W`->`W` (cycle layout), `P`->`P` (float/tile), `B`->`B` (flatten), `A`->`S` (flip
   split), `G`->`;` (manage), `Z`->`G` (prev ws), `X`->`Tab` (bounce monitor),
   `C`->`R` (reload), `D`->`-`, `V`->`=` (resize).
6. **Mode entries** (leader chords, in the WM layer, `+Super`): `Space` -> `Super+Space`
   (launch mode, then type the app letter on the base layer); `G` -> `Super+;`
   (manage mode). No F-keys involved.
7. **One-tap utility keys** (unchanged firmware; `KC_Fxx` + Left Ctrl+Alt+GUI+Shift =
   `Hyper + Fxx`): `Hyper + F13` -> Key Lights, `Hyper + F14` -> Screen Saver. Your
   existing F13/F14 keys already emit these and just regain their one-tap meaning via
   AeroSpace.
8. Compile, download the new `.bin`, flash via Keymapp.

Raw Hyper remains on the left-pinky `Esc`/`Hyper` key (`ALL_T(KC_ESCAPE)`).

### Step 5 - Verify on both keyboards

Walk the cheat sheet on the Moonlander and the built-in. Confirm identical behaviour
(same `hjkl`, same digits, Shift = move on both).

## Open items (still to decide)

- **`config-version`** - AeroSpace warns v1 is outdated; migrating to v2 is a separate,
  behaviour-affecting pass (some option semantics change).
- **Delete disabled Alfred workflows** - do this once the whole scheme (incl. firmware)
  feels right; they are currently disabled, not deleted.

## Decisions locked (context for a fresh session)

- Leader = `Super` (Ctrl+Opt+Cmd). Move/send tier = add `Shift`. Same on both boards.
- Built-in leader = Caps Lock (hold=Super, tap=Esc) via Karabiner.
- Moonlander leader = left thumb, repurposed from `KC_HYPR` to a momentary WM layer;
  layer keys emit `Super + <key>`; move = physical Shift.
- Directions = `hjkl` (left/down/up/right); workspaces = digits. Position-based, so
  Colemak vs QWERTY is irrelevant.
- **Moonlander hands swapped vs built-in:** directions on the **left** hand (inverted-T
  arrow shape), workspaces on the **right** hand - to match existing arrow muscle memory.
  Built-in keeps `hjkl` on the right hand. Keycodes/cheat sheet identical; only the hand
  differs.
- **Workspaces = 5 numbered** (1-5). **Workspace 5 pinned** to the built-in display
  (`workspace-to-monitor-force-assignment`, matched by `'built-in'`).
- Ultrawide = tiles, 2-3 columns. MacBook (physically left) hosts workspace 5.
- Alfred Hyper launchers retired: apps -> AeroSpace **launch mode** (`Super + Space` =
  `Caps + Space` / WM-layer `Space`); Key Lights + Screen Saver -> **one-tap** `exec` on
  `Hyper + F13/F14`. Alfred workflows disabled (not deleted).
- Sequencing: built-in path first (reversible, no flashing); firmware last.
```
