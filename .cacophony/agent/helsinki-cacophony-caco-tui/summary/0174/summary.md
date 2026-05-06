# bd-059fe5: skip single upload candidate sort

## What changed

- `SurfaceManager::pending_uploads_with_summary()` now guards the final `sort_unstable_by` with `if result.len() > 1`.
- Zero/single regular-upload candidate frames skip sort comparator/key work after budget selection.
- Multi-candidate frames still sort by first-upload priority, precomputed upload rank, and key tiebreaker for deterministic ordering.
- Added focused runtime/source coverage for the single-candidate fast path and multi-candidate ordering.
- Updated existing source-shape coverage to account for the uniform-rank reuse expression.

## Why

Many cached/retained Kitty frames only have one surface to redisplay/upload. Sorting a one-element vector is logically unnecessary but still enters sort setup/comparator plumbing. This is a small hot-path reduction in the regular upload collector while preserving deterministic behavior for bursts.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/kitty.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_059fe5"` — `tj-d77c6091`, passed
- `caco test run --wait --command "cargo test -p caco-tui pending_upload"` — `tj-5ee3d999`, passed

Earlier broad `pending_upload` run `tj-53e52b9e` failed because an existing source-shape assertion expected the old explicit rank expression; the implementation and focused test were already passing, then the assertion was updated and the suite passed.
