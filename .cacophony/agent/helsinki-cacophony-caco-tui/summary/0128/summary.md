# bd-b886f0: collapse phase-cache border liveness pre-scan

## What changed

- Removed the animation phase-only `cached_panel_surfaces_live()` pre-scan before the phase-cache fast path in `BorderIntegration::register_panel()`.
- The phase-cache refresh loop already calls `refresh_registered_animation_frame()` per required segment and falls back to full registration if any segment is missing, so correctness is preserved without the duplicate pre-scan.
- Removed the now-unused `cached_panel_surfaces_live()` helper.
- Added source-shape coverage to ensure the phase setup does not reintroduce a pre-scan and that the phase-cache loop still falls back on missing segments.

## Why

Animated graphics borders should be close to text-mode cost once cached. The old phase path hashed/probed every required segment once to check liveness and then probed the same segments again to refresh cached animation frames. Eliminating the pre-scan removes duplicate `HashMap` work on every cached animated-border frame while relying on the existing fallback path for stale/missing surfaces.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/border_integration.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_b886f0"` — `tj-982156fb`, passed with now-unused helper warning.
- Removed the unused helper and reran `caco test run --wait --command "cargo test -p caco-tui bd_b886f0"` — `tj-90610fca`, passed cleanly.
