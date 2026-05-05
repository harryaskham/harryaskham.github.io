# bd-6cd101: skip tmux pane-origin refresh on TUI Kitty delete/fetch-only passes

## What changed

- The live Kitty graphics upload pass no longer refreshes the tmux pane-origin cache at the start of every graphics-capable pass.
- In tmux passthrough mode, `refresh_tmux_pane_origin()` is now deferred until the pass is about to emit cursor-positioned Kitty placement commands:
  - native animation uploads, or
  - regular full/retained placement uploads.
- Delete-only, animation-stop-only, fetch-only, and backoff-only passes keep their existing cleanup/fetch behavior without paying for a pane-origin query.
- Added a regression test guarding that origin refresh is tied to pending placement work rather than unconditional per-pass work.

## Why

Kitty placement commands need the tmux pane origin because cursor moves are interpreted in outer-terminal coordinates. Delete commands, animation-stop commands, async fetch initiation, and backoff ticking do not use cursor placement. Refreshing the origin on those passes added avoidable per-frame tmux query overhead while contributing nothing to drawing correctness.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_6cd101"` — `tj-a5d56077`, passed
