# Goal

Reduce unnecessary graphics upload-path bookkeeping so native-animation retry state does not force native-ready recount work during ordinary regular bitmap upload scans.

# Bead(s)

- `bd-14e7bb` — Track native-animation backoff separately from regular upload scans.

# Before state

After `bd-f32afe`/`bd-855e3a`, native-animation upload collection was bounded by native-key and native-ready counters. However, `pending_uploads_with_summary()` still used a generic `ticked_backoff` flag for all surfaces. If any non-native bitmap surface had backoff decremented during regular upload collection, the code recomputed native-animation ready-upload bookkeeping even though no native surface changed.

# After state

Regular upload collection now distinguishes native backoff ticks from generic backoff ticks. The native-ready recount runs only when a surface with `native_animation.is_some()` actually had its backoff counter decremented. Non-native backoff countdowns remain correctly ticked and skipped for that cycle, but they no longer trigger native-ready recount work.

# Diff summary

- Replaced the generic upload-collection `ticked_backoff` flag with `ticked_native_backoff`.
- Set the recount flag only when a backoff-ticked surface is a native-animation surface.
- Kept native-ready recounts after native backoff ticks in both unlimited-budget and finite-budget upload paths.
- Updated the existing backoff source-shape regression for the new native-specific recount flag.
- Added `native_backoff_recount_is_separate_from_regular_backoff_bd_14e7bb` to prove the regular upload path tracks native backoff separately from non-native backoff.

# Validation

- `cargo test -p caco-tui bd_14e7bb` — `tj-5a6e4815`
- `cargo test -p caco-tui pending_upload_ticks_backoff_during_collection_bd_313ae0` — `tj-6aaf2136`
- `cargo test -p caco-tui bd_855e3a` — `tj-0e0e3147`
- `cargo clippy -p caco-tui --lib -- -D warnings` — `tj-e046dd57`
- `cargo test -p caco-tui` — `tj-0ba1ce47`
- `rustfmt --edition 2021 crates/caco-tui/src/kitty.rs`
- `git diff --check`

Note: a malformed multi-filter cargo invocation failed before the corrected focused runs: `tj-e3c64507` (`cargo test` accepts a single test filter). The corrected runs above passed.

# Operator-takeaway

This is a small graphics hot-path reduction with no intended visual behavior change. It keeps native animation retry/readiness correctness while avoiding native-ready recount work when only regular bitmap surfaces are ticking upload backoff.
