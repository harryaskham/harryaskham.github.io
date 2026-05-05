# bd-dc07a0: preserve TUI background cache correctness for match-border tint

## What changed

- `BackgroundSurfaceSnapshot` now includes `border_fg_for_match_tint` only when a resolved background layer uses active `tint: match_border` with non-zero tint intensity.
- Normal border foreground/focus changes still do not invalidate the background fast path after bd-788ca0.
- Match-border tinted backgrounds correctly invalidate when border foreground changes, preserving visual correctness.
- Added regression coverage for active match-border tint vs zero-intensity match-border tint.

## Why

bd-788ca0 intentionally stopped full background snapshot invalidation for border-only changes to keep the app-level background cache hot across focus/title-border churn. That is correct for ordinary backgrounds, but `resolve_tint_rgba()` uses `request.fg` for `BackgroundTint::MatchBorder`. This follow-up keeps the performance win for ordinary themes while restoring correctness for themes where the background pixels actually depend on border foreground color.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_dc07a0"`
  - `tj-eacd04af` failed with compile errors while wiring the helper/test.
  - First retry hit transient daemon reachability.
  - `tj-d175a32c` passed after fixing the helper and test clones.
