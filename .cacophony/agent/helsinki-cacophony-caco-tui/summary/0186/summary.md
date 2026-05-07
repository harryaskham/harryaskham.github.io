# bd-f32afe — Skip native-animation collection when no native uploads are pending

## Summary

Added a native-animation ready-upload counter to the Cacophony TUI graphics surface manager so non-animation frames can skip native-animation upload collection entirely when all terminal-native loops are already uploaded or still in retry backoff.

This continues the graphics-vs-ASCII overhead reduction work: after `bd-d5e637` narrowed native upload scans to native surface keys, `pending_native_animation_uploads()` could still walk that key set on ordinary frames where no native upload could be emitted. This slice adds bounded readiness bookkeeping to avoid that scan in the steady state.

## Changes

- Added `native_animation_ready_upload_count` to `SurfaceManager`.
- Added `native_animation_ready_for_upload()` and recomputation helper for non-animation upload readiness.
- Updated readiness bookkeeping when native upload state changes via:
  - native/static/native mode transitions,
  - successful upload marking,
  - upload failure/backoff,
  - explicit invalidation/reassertion,
  - clear/retire,
  - standalone backoff ticking,
  - regular upload collection that also ticks native backoff counters.
- Updated `pending_native_animation_uploads()` to return before iterating native keys when the current pass is not an animation redraw and no native upload is ready.
- Added focused coverage for:
  - skipping native collection when uploaded native loops are not ready,
  - readiness recovering after invalidation,
  - readiness clearing/recovering around native upload backoff,
  - readiness updating when regular upload collection ticks native backoff counters.
- Updated an existing source-shape regression test so it still verifies backoff ticking in the upload collection path after the new readiness refresh flag.

## Validation

Local validation before checkpoint:

- `cargo test -p caco-tui bd_f32afe` — `tj-cd339538`
- `cargo test -p caco-tui pending_upload_ticks_backoff_during_collection_bd_313ae0` — `tj-5a748c2e`
- `cargo test -p caco-tui reassert_managed_surfaces_reuploads_native_animation_without_deletes` — `tj-5f43094c`
- `cargo clippy -p caco-tui --lib -- -D warnings` — `tj-e64daf64`
- `cargo test -p caco-tui` — `tj-e9bc39fc`

Earlier focused validation while iterating:

- `cargo test -p caco-tui bd_f32afe` — `tj-6d5f288f`, `tj-0f396e1a`
- `cargo test -p caco-tui pending_native_animation` — `tj-c330cb21`
- Initial full test failure `tj-cbd5fd1e` exposed stale source-shape/reassertion assumptions; those were fixed before the passing full test above.

`rustfmt --edition 2021 crates/caco-tui/src/kitty.rs` and `git diff --check` completed cleanly.

## Current lifecycle note

Work is checkpointed locally while the ms-mac caution window is active. Rebase/revalidation/reintegration/closeout are intentionally held until caco-ctrl/cluster-ctrl clears ms-mac-sensitive actions and daemon/canonical checkout convergence is verified.

## SPEC coverage

Preserves SPEC 20.7/20.8 graphics contracts: terminal graphics remain ratatui-owned enhancements; native animation uploads keep dedicated queue semantics, upload-budget ordering, invalidation/reupload correctness, and fallback behavior. The change only reduces unnecessary upload-collection work when no native upload can be emitted.
