# bd-f16c73: avoid hashing modal-suppressed TUI panels

## What changed

- `BorderIntegration::suppress_for_modals()` now uses a per-panel `panel_suppressed` flag and records each suppressed panel ID once after segment/title checks.
- Replaced the `HashSet<String>` used for suppressed panels with a `Vec<String>` and removed the now-unused `HashSet` import.
- Retire/de-draw behavior for overlapping segments and title decorations is unchanged; only panel-state cleanup bookkeeping changed.
- Added source-shape coverage that modal suppression no longer calls `suppressed_panels.insert(...)` per segment/title.

## Why

A base panel can have multiple border segments and a title decoration overlapping a modal. The old path inserted/cloned the same `panel_id` into a `HashSet` for every suppressed surface before removing panel state. Tracking a bool per panel and pushing once avoids repeated hashing/cloning on modal overlay frames while preserving de-draw correctness.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/border_integration.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_f16c73"` — `tj-c88c423c`, passed but showed unused import warning
- Removed unused `HashSet` import and reran `caco test run --wait --command "cargo test -p caco-tui bd_f16c73"` — `tj-4ef27d3d`, passed cleanly
