# claude-skills

Installs Claude Code skills from pinned flake inputs, plus the binary/runtime
dependencies each skill needs. Skill sources currently wired up:

- [`anthropics/skills`](https://github.com/anthropics/skills) — flake input `claude-skills`
- [`jgraph/drawio-mcp`](https://github.com/jgraph/drawio-mcp) — flake input `drawio-skill`
- [`herdrdev/herdr`](https://github.com/herdrdev/herdr) — flake input `herdr-skill`
- Local `modules/ai/claude-skills/mermaid/` — custom skill, no external source

Skills are symlinked into `~/.claude/skills/<name>` (Nix store path,
read-only), so they bump in lockstep with their flake input.

## Enabling a skill

In your machine's `home.nix`:

```nix
ai.claude-skills.docx.enable = true;
```

## Available skills

### drawio (`drawio.nix`)

Generates native `.drawio` diagram files; can export to PNG/SVG/PDF via
draw.io Desktop.

Pulls in:
- The `SKILL.md` from `jgraph/drawio-mcp` `skill-cli/drawio/`

Also requires the draw.io Desktop app for image export. Add the cask in your
machine's `casks.nix`:

```nix
"drawio"
```

### docx (`docx.nix`)

Word document creation, editing, and analysis.

Pulls in:
- `pandoc`, `poppler-utils` (system binaries)
- `python312Packages.defusedxml` (folded into the python wrapper via `language.python.extraPackages`)
- `docx` npm library (vendored via `pkgs.buildNpmPackage`, exposed through `NODE_PATH`)

Also requires LibreOffice for `.doc → .docx` conversion, PDF rendering, and
accepting tracked changes. Add the cask in your machine's `casks.nix`:

```nix
"libreoffice"
```

### herdr (`herdr.nix`)

Lets Claude drive the [herdr](https://herdr.dev) agent multiplexer: manage
workspaces, split panes, run commands, read pane output, and coordinate with
neighbouring agents via the `herdr` CLI.

Pulls in:
- The `SKILL.md` from `herdrdev/herdr` `skills/herdr/`

Requires the `herdr` CLI, which comes from the separate `herdr` module
(`herdr.enable = true;`). The skill only activates meaningfully when Claude is
launched from inside a herdr pane, where `HERDR_ENV=1` and the local herdr
socket are available.

### mermaid (`mermaid.nix`)

Generates `.mmd` mermaid source files and renders them to SVG/PNG/PDF using
[`mermaid-cli`](https://github.com/mermaid-js/mermaid-cli) (`mmdc`).

Pulls in:
- The `SKILL.md` from `modules/ai/claude-skills/mermaid/` (local, vendored
  in this repo since upstream skills don't know about Nix).

No system packages or casks needed. The skill invokes
`nix run nixpkgs#mermaid-cli` on demand, which fetches and runs `mmdc`
(including its puppeteer/chromium dependency) from nixpkgs. First
invocation has download latency and needs network; subsequent runs are
cached in the Nix store.

Note: `nix run nixpkgs#…` resolves against the flake registry's nixpkgs,
not this repo's pinned `nixpkgs`. Acceptable for v1; switch to
`github:NixOS/nixpkgs/<rev>#mermaid-cli` if lockstep is needed.

## First-time setup

If `~/.claude/skills/<skill>` already exists as a real directory (not a
home-manager symlink), home-manager refuses to overwrite it. Remove it before
the first `./bin/switch`:

```sh
rm -rf ~/.claude/skills/docx
```

## Bumping a skill source

```sh
nix flake update claude-skills   # anthropics/skills (docx, etc.)
nix flake update drawio-skill    # jgraph/drawio-mcp (drawio)
nix flake update herdr-skill     # herdrdev/herdr (herdr)
./bin/switch
```

## Bumping a vendored npm dependency (docx)

1. Edit `docx-npm/package.json` to the new version range.
2. Regenerate the lockfile:
   ```sh
   cd modules/ai/claude-skills/docx-npm
   npm install --package-lock-only --ignore-scripts
   ```
3. Run `./bin/switch`. The build fails with a hash mismatch — copy the `got:`
   value from the error into `npmDepsHash` in `docx.nix` and switch again.

## Adding a new skill

1. Create `modules/ai/claude-skills/<skill>.nix` following the pattern in
   `docx.nix` or `drawio.nix`: declare an `ai.claude-skills.<skill>.enable`
   option, install deps, symlink the skill from the appropriate flake input
   (`${inputs.claude-skills}/skills/<skill>` for anthropics, or add a new
   non-flake input for an external repo), and register `verify.checks` for
   each binary.
2. Import it from `default.nix`.
3. Enable it in the machine's `home.nix`.
