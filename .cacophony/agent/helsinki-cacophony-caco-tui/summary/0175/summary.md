# bd-34a33a: skip single native animation candidate sort

## What changed

- `SurfaceManager::pending_native_animation_uploads()` now guards the final `sort_unstable_by` with `if result.len() > 1`.
- Zero/single native-animation upload candidate frames skip sort comparator/key work after budget selection.
- Multi-candidate native frames still sort by prior-upload state, missing-placement priority, upload rank, and key tiebreaker for deterministic ordering.
- Added focused runtime/source coverage for the single-candidate fast path and multi-candidate ordering.
- Updated existing native upload source-shape coverage to match the already-current budget-truncate-before-sort behavior and `sort_unstable_by` naming.

## Why

Native Kitty animation upload is usually a one-time placement for a single animated surface. Sorting a one-element candidate list is unnecessary but still pays comparator/setup overhead. This mirrors the regular upload fast path and trims another small graphics upload hot path without changing multi-candidate correctness.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/kitty.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_34a33a"` — `tj-ae28405f`, passed
- `caco test run --wait --command "cargo test -p caco-tui pending_native"` — `tj-919fab6e`, passed

Earlier focused run `tj-b3224d1b` passed; `tj-1bdf88d5` only matched the pre-existing native-animation budget test. A broader `pending_native` run (`tj-bd8ee9e0`) exposed an outdated source-shape assertion, which was updated before the final passing run.
