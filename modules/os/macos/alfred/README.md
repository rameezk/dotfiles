# Alfred workflows

Alfred workflows are `.alfredworkflow` bundles (a zip of a folder containing an
`info.plist`). Alfred owns them at runtime, so they are not managed by Nix. The
source lives here and is packaged into an importable bundle.

## Key Lights

Toggles the study key lights by running the macOS Shortcut `Toggle Key Lights`
via the Shortcuts background service (the Shortcuts app does not need to be
open):

    /usr/bin/shortcuts run "Toggle Key Lights"

- Trigger: global hotkey **Hyper (⌃⌥⇧⌘) + F13** (bind a single Moonlander key to
  Hyper + F13 in Oryx).
- Source: `keylights/info.plist`
- Bundle: `Key Lights.alfredworkflow`

### Import

Double-click `Key Lights.alfredworkflow`, then click **Import** in Alfred. To
confirm the hotkey imported, open the workflow and check the Hotkey object reads
`F13`.

### Rebuild the bundle after editing the source

    ./build.sh
