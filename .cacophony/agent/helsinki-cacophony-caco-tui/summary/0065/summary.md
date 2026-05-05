# bd-75791e: fold skipped animated upload counting into TUI Kitty pending-upload scan

## What changed

- `SurfaceManager::pending_uploads()` now counts skipped cached animated surfaces during its initial surface-map walk.
- Removed the follow-up full `self.surfaces.iter()` scan that previously existed only to update `upload_skipped_count` diagnostics on non-animation redraws.
- Preserved `upload_skipped_count` semantics for animated cached surfaces that are intentionally held until an animation redraw.
- Added a regression test guarding against the old second-scan shape.

## Why

The regular Kitty upload candidate path already inspects every surface to find eligible uploads. On non-animation redraws it then scanned every surface again solely to count cached animated surfaces skipped by the animation gate. Folding that diagnostic count into the existing collection pass removes an avoidable per-frame map walk while keeping the diagnostic signal intact.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/kitty.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_75791e"` — `tj-a2f0caba`, passed
