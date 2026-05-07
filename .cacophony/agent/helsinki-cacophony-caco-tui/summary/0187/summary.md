# Goal

Reduce residual Cacophony TUI graphics overhead on the native-animation upload path by avoiding native-ready recount work when unrelated non-native surfaces change upload or backoff state.

# Bead(s)

- `bd-855e3a` — Limit native-ready recounts to native surface changes.

# Before state

`bd-f32afe` added `native_animation_ready_upload_count` so non-animation frames can skip native-animation upload collection when no native loop is ready. That was correct, but the first implementation refreshed the ready count from generic upload/backoff state changes even when the changed surface was not a terminal-native animation surface. In mixed scenes, static bitmap uploads/failures could therefore pay unnecessary native-ready recount work.

# After state

`SurfaceManager` now has a key-scoped helper that recomputes native-animation ready-upload bookkeeping only when the changed key is currently tracked as a native-animation surface. Generic `mark_uploaded_with_image_id()` and `mark_upload_failed()` use that bounded helper, while broader lifecycle paths that can affect many native surfaces (invalidate/reassert/retire/clear/backoff ticking) still preserve correctness by refreshing or clearing the native-ready count as appropriate.

# Diff summary

- Added `recompute_native_animation_ready_upload_count_if_native(key)`.
- Switched generic upload success/failure paths to use the key-scoped helper.
- Preserved full recounts on native mode transitions, invalidation/reassertion, retire/clear, and upload collection/backoff ticking paths that can affect native readiness.
- Added `native_ready_recount_skips_non_native_upload_state_changes_bd_855e3a` to prove non-native upload/failure changes do not disturb native readiness bookkeeping.

# Validation

- `cargo test -p caco-tui bd_855e3a` — `tj-b707e323`
- `cargo test -p caco-tui bd_f32afe` — `tj-046aa334`
- `cargo test -p caco-tui pending_upload_ticks_backoff_during_collection_bd_313ae0` — `tj-7bec9624`
- `cargo clippy -p caco-tui --lib -- -D warnings` — `tj-e37b2b5d`
- `cargo test -p caco-tui` — `tj-f9f7072e`
- `rustfmt --edition 2021 crates/caco-tui/src/kitty.rs`
- `git diff --check`

# Operator-takeaway

This is another small graphics hot-path trim: non-native image upload state changes no longer force native-animation ready recounts. It should not change visible behavior; it only narrows bookkeeping to native surfaces when possible.
