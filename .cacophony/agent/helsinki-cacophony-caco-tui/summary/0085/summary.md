# bd-be9d94: reuse cached border surface keys during modal suppression

## What changed

- `BorderIntegration::suppress_for_modals()` now reuses cached `panel_surface_keys` when retiring overlapped border segments instead of re-formatting a segment surface key for each overlap.
- Suppressed panels now drop `panel_surface_keys` along with snapshots, render plans, phase caches, and generation tracking.
- Added regression coverage ensuring modal suppression removes cached segment keys for suppressed base panels.

## Why

Modal overlays can suppress base-layer bitmap border segments on frames where overlay chrome appears. The border integration already keeps stable per-panel surface keys for hot-path border drawing, but the modal suppression path recomputed those strings. Reusing the cached keys avoids repeated formatting/allocation and keeps cached panel bookkeeping consistent after suppression, while preserving correct de-draw of overlapped segments and preserving modal/toast panel borders.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/border_integration.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_be9d94"` — `tj-9d65bf6b`, passed
