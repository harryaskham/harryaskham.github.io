# bd-d5e637 — Limit native-animation upload scans to native surface keys

## Summary

Continued the TUI graphics performance burndown by narrowing the native-animation upload hot path. After `bd-a22dd3` added an empty-native fast path, frames with even one terminal-native animation still scanned every registered graphics surface during native upload preflight and collection. This slice adds a bounded native-surface key set so those paths inspect only surfaces that can actually hold a Kitty/Ghostty native animation payload.

## Changes

- Added `native_animation_surface_keys` alongside `native_animation_surface_count` in `SurfaceManager`.
- Updated native-animation transition bookkeeping to insert/remove keys only when a surface enters/leaves terminal-native animation mode.
- Kept key tracking aligned across daemon image reservation, static enhancement registration, animated bitmap enhancement registration, movable enhancement registration, native animation registration, refresh fast paths, `retire()`, and `clear()`.
- Changed `has_pending_native_animation_uploads()` to scan `native_animation_surface_keys` rather than all `surfaces` once native loops exist.
- Changed `pending_native_animation_uploads()` to collect from `native_animation_surface_keys`, preserving existing budget, priority sort metadata, deterministic ordering, post-budget cloning, and upload eligibility semantics.
- Updated source-shape tests to reflect the new native-key scan path while still asserting the existing clone-after-budget and metadata-reuse contracts.

## Validation

Post-rebase validation passed:

- `cargo test -p caco-tui bd_d5e637` — `tj-bc74d5f3`
- `cargo test -p caco-tui pending_native_animation` — `tj-a62e3373`
- `cargo clippy -p caco-tui --lib -- -D warnings` — `tj-e7260946`
- `cargo test -p caco-tui` — `tj-f875b5dc`

Earlier pre-rebase validation / diagnostics:

- `cargo test -p caco-tui bd_d5e637` — `tj-0709ddb6`
- `cargo clippy -p caco-tui --lib -- -D warnings` — `tj-28a5471f`
- Initial full test failure `tj-d8d06b60` exposed source-shape assertions that expected the old all-surface native scan; the assertions were updated and full tests then passed.
- `cargo test -p caco-tui pending_native_animation` — `tj-940c8f37`
- `cargo test -p caco-tui bd_d5e637` — `tj-8156d46c`
- `cargo test -p caco-tui pending_upload_sorts_reuse_collected_surface_metadata_bd_500769` — `tj-9c6a6e9e`
- `cargo test -p caco-tui` — `tj-ffa251f8`
- Final pre-commit `cargo clippy -p caco-tui --lib -- -D warnings` before rebase — `tj-82d5ee71`

`rustfmt --edition 2021 crates/caco-tui/src/kitty.rs` and `git diff --check` completed cleanly before commit.

## SPEC coverage

Preserves SPEC 20.7/20.8 graphics contracts: terminal graphics remain ratatui-owned enhancements, native animation uploads keep their dedicated queue and upload-budget ordering, and the upload pass remains demand-driven. This change only reduces unnecessary scan scope; it does not change layout, placement identity, z-ordering, fallback behavior, or visible output.
