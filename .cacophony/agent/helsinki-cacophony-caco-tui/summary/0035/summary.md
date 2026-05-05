# bd-e8f66a: skip TUI border stale-panel scan when all panels are live

## What changed

- Added `current_frame_live_panel_count` to `BorderIntegration`.
- `begin_frame()` resets the count; `register_panel()` increments it only when a distinct tracked panel is first seen in the current frame.
- `end_frame()` now fast-returns when the live count and generation map match the snapshot count, avoiding the stale-panel scan on all-live steady-state frames.
- Stale cleanup is preserved when a panel disappears because the live count is then lower than the snapshot count.
- Added regression coverage for the all-live fast path.

## Why

`BorderIntegration::end_frame()` scanned all panel snapshots every frame to find stale border surfaces. Most steady-state graphics frames touch every tracked panel and remove nothing. The live-count fast path avoids that O(panel_count) scan while preserving border de-draw correctness when panels close or views change.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/border_integration.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_e8f66a"` — `tj-e89a4b62`, passed
