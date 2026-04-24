# Session summary — kitty graphics in workspace-view terminal panes (bd-ca37fc)

## Goal
Web terminal panes can render inline images emitted by `caco --gfx`
(bd-82f4b0) using the kitty graphics protocol. Subset implementation:
parse APC sequences off the WS stream, decode RGB/RGBA/PNG payloads,
overlay onto the terminal host with a 50 MiB LRU memory cap.

## Bead(s)
- bd-ca37fc — [workspace-view V2] Kitty-graphics image rendering in
  terminal panes
- parent epic: bd-027e9d (caco-web Workspace View V2)
- consumes: bd-82f4b0 (caco --gfx flag origin) — this is the receive
  half of the protocol bd-82f4b0's emit half established.

## Before state
- Failing tests: none.
- Web terminal panes received WS bytes via xterm-addon-attach
  AttachAddon and pumped them straight into xterm. Any kitty graphics
  APC sequence (`\\x1b_G...\\x1b\\\\`) rendered as garbled control
  characters in the terminal.
- No image-rendering primitive in any caco-web JS module.

## After state
- Failing tests: none. `cargo test -p caco-web --lib` = 166 passed (+3).
- Pre-existing clippy warnings on a11y_lint.rs unchanged (not in scope).
- New static asset: kitty-graphics.js (~430 lines, fully self-contained,
  no deps).
- workspace-terminal-pane.js intercepts WS frames when KittyGraphics is
  loaded; falls back to AttachAddon otherwise (acceptance #2).
- 50 MiB per-terminal LRU memory cap (acceptance #3) enforced in
  KittyGraphicsRenderer.
- Smoke test exercises 7 protocol scenarios under node.

## Diff summary
- New: crates/caco-web/static/kitty-graphics.js (~430 lines)
- Modified: crates/caco-web/static/workspace-terminal-pane.js
  (+~80 lines: per-pane filter+renderer+overlay; conditional
  AttachAddon bypass)
- Modified: crates/caco-web/static/index.html (+1 line: load
  kitty-graphics.js before workspace-terminal-pane.js)
- Modified: crates/caco-web/src/tests.rs (+3 tests)
- Tests: +3 / -0
- Behavioural delta: web terminal panes; no daemon-side change.

## Operator-takeaway
Inline images now render in the workspace-view terminal panes. The
implementation is intentionally a subset of the full kitty spec —
enough for `caco --gfx` payloads. Full cell-aligned grid positioning,
animation frames, and image compression are explicit out-of-scope and
should land as follow-up beads if/when an operator wants them. The
filter/renderer split is clean enough that the same two modules can
be reused unchanged inside the standalone /terminal.html surface
(currently still on AttachAddon) — single-line load + same wiring.

Pre-close audit (per close-discipline directive): will run
`git log origin/main --grep=bd-ca37fc` after reintegrate; only close
if the bd-id reaches main with my commit footer attached.
