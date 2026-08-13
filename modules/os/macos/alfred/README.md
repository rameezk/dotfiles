# Alfred workflows

Each workflow is a folder here containing an `info.plist`. The folders are
symlinked into Alfred's workflows directory so the repo is the live source of
truth and Alfred writes edits back to it.

## Why symlink instead of import

Alfred **strips hotkeys when a workflow is imported** (double-clicking a
`.alfredworkflow` bundle), so imported workflows always come in with blank
hotkeys. Placing the folder directly in Alfred's workflows directory preserves
the hotkeys stored in `info.plist`. `install.sh` does this by symlinking each
workflow folder to:

    ~/Library/Application Support/Alfred/Alfred.alfredpreferences/workflows/user.workflow.dotfiles-<name>

## Installation

Managed by the `macos.alfred` module. Enabling it runs `install.sh` on every
`darwin-rebuild switch` (via a home-manager activation script):

    macos.alfred.enable = true;

`install.sh` is idempotent: it links any workflow folder that has an
`info.plist`, prunes `dotfiles-*` links whose source no longer exists, and
relaunches Alfred only when something changed. It can also be run by hand.

Adding a new workflow is just a new folder with an `info.plist`; the next switch
picks it up automatically.

## Key Lights

Toggles the study key lights by running the macOS Shortcut `Toggle Key Lights`
via the Shortcuts background service (the Shortcuts app does not need to be
open):

    /usr/bin/shortcuts run "Toggle Key Lights"

- Trigger: global hotkey **Hyper (⌃⌥⇧⌘) + F13** (bind a single Moonlander key to
  Hyper + F13 in Oryx).
- Source: `keylights/info.plist`

## launcher

Launches (or focuses) apps via global **Hyper (⌃⌥⇧⌘) + letter** hotkeys, plus one
system command:

| Hotkey | Action |
| --- | --- |
| Hyper + A | Claude |
| Hyper + B | Google Chrome |
| Hyper + C | Microsoft Teams |
| Hyper + E | IntelliJ IDEA |
| Hyper + L | Todoist |
| Hyper + M | Safari |
| Hyper + N | Obsidian |
| Hyper + O | Microsoft Outlook |
| Hyper + S | Slack |
| Hyper + T | WezTerm |
| Hyper + F14 | Start screensaver |

- Source: `launcher/info.plist`

## Shareable bundles

`build.sh` packages the folders into `.alfredworkflow` bundles for sharing or
importing elsewhere. Note that importing a bundle loses hotkeys (see above), so
this is only for distribution, not the local install path.

    ./build.sh
