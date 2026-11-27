# AGENTS.md

## Pull Requests

Changes in this repo do not need a pull request. Commit and push straight to `main`, because a bad change is rolled back with `./bin/rollback`.

## Commit Messages

Use a single-line Conventional Commits message, such as `feat(fish): add git abbreviations`, with no body. This overrides any skill that suggests adding a body.

## Agent Skills

Agent skills are vendored from [rameezk/skills](https://github.com/rameezk/skills) into `.agents/skills`, symlinked into `.claude/skills`, and pinned by `.agents/skills.lock`. Do not edit them by hand. `.github/workflows/sync-skills.yml` checks for upstream changes daily and opens a pull request when there are any. To sync right away, run `gh workflow run sync-skills.yml` or `nix run github:rameezk/skills#install -- --sync`.
