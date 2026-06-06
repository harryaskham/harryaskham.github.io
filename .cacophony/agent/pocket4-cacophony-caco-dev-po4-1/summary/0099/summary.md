# Session summary — Shift-T grid mouse routing fix

## Goal

Fix the TUI Shift+T grid tab layout so mouse clicks go to the grid cell/pane the operator actually clicked, matching manual split-pane behavior and avoiding clicks in one visible pane being routed to another pane.

## Bead(s)

- `bd-2ecb33` — Fix mouse handling in grid mode (shift-T)

## Before state

- In grid tab layout, multiple tab contents were rendered visibly inside one workspace tile.
- Mouse dispatch still treated the workspace tile as a single active content pane; attached PTY/tmux mouse forwarding could run before grid-cell hit-testing.
- Reported symptom: clicking the top-left grid pane could interact with a different pane such as the bottom-left pane.

## After state

- Grid mode records frame-local per-cell outer and inner rectangles for each rendered tab.
- Mouse-down inside a grid cell switches focus/active tab to that cell before attached PTY/tmux forwarding.
- Stale input attachments are detached when a click crosses to another grid cell, so subsequent normal mouse handling can attach or interact with the correct visible pane.
- Embedded shell/agent terminal origin and size calculations use the active grid cell's inner rectangle while grid mode is active, aligning SGR mouse coordinates with the drawn cell.

## Diff summary

- Code/content commits: `cb1a1e0ff` (final landed squash SHA will come from the reintegration receipt).
- Files touched: `crates/caco-tui/src/app.rs`.
- Tests: added 2 focused unit tests:
  - `grid_cell_click_selects_cell_tab_before_mouse_forwarding_bd_2ecb33`
  - `grid_shell_embed_geometry_uses_active_cell_bd_2ecb33`
- Validation: `RUST_MIN_STACK=33554432 cargo test -p caco-tui bd_2ecb33 -- --test-threads=1` passed (2 tests).
- Formatting note: `./scripts/rustfmt-changed.sh crates/caco-tui/src/app.rs` intentionally skipped because HEAD's `app.rs` is not rustfmt-clean; I manually wrapped long lines in the touched blocks and `git diff --check` passed.

## Embedded artefacts

None.

## Operator-takeaway

The bug was a second-level hit-testing gap: Shift+T grid mode draws many visible panes inside one tile, but mouse dispatch and embedded-terminal coordinate translation still used the single active tab/tile model. The fix adds grid-cell hit maps and grid-aware embed geometry so a click in a visible grid cell targets that cell, not whichever tab was active before.
