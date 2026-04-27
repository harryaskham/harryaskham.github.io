# Session summary — Fullscreen hit-test routing

## Goal

Fix Harry's report that, in TUI fullscreen mode, button clicks and other mouse targets can still route through the old split layout instead of the visible fullscreen panel.

## Bead(s)

- `bd-1cbc8c` — TUI fullscreen button clicks use stale split layout hit targets

## Before state

- Failing tests: none known for this path.
- Relevant metrics: no FPS benchmark run; this was a mouse hit-testing correctness fix.
- Context: older fullscreen protection skipped some split-layout focus/drag hit tests, but the workspace's cached tile rects still represented the prior split layout while fullscreen rendered only the focused tile into the full visible area. Button hit maps also persisted across frames, so hidden panes could leave stale absolute hit rectangles around.

## After state

- Failing tests: none in targeted validation.
- Relevant metrics: focused fullscreen test subset passed with cargo limited to two jobs.
- Context: fullscreen rendering now writes a frame-local workspace hit-test snapshot where the focused tile occupies the rendered fullscreen area, while keeping the normal split layout dirty so it recomputes after exiting fullscreen. Frame start also clears button hit maps and hover state so hidden panes cannot retain stale button hit targets.

## Diff summary

- Commits: `4f71b454d`
- Files touched: `crates/caco-tui/src/app.rs`, `crates/caco-tui/src/workspace.rs`
- Tests: +1 regression test / -0 / flipped 0
- Behavioural delta: fullscreen mouse hit-testing is now based on the visible focused tile, not the cached underlying split geometry.
- Validation:
  - `CARGO_BUILD_JOBS=2 cargo test -j2 -p caco-tui fullscreen_render_updates_workspace_hit_rect_to_focused_tile --lib`
  - `CARGO_BUILD_JOBS=2 cargo test -j2 -p caco-tui fullscreen_ --lib`

## Operator-takeaway

Fullscreen should now behave like the visible panel is the only mouse target: clicks in the enlarged area no longer resolve against hidden split panes or stale button rectangles.
