# bd-e5a0d1: avoid unconditional image-cache probes in TUI Kitty pending-work summary

## What changed

- `SurfaceManager::pending_graphics_work_summary()` now makes the cache-needing classification cases explicit:
  - `needs_fetch_check`,
  - `needs_regular_upload_check`.
- `image_cache.contains_key(key)` is evaluated lazily only when at least one of those classifications can use it.
- The previous bd-342d88 one-probe behavior is preserved when both fetch and regular-upload checks apply to a surface.
- Native-animation, uploaded, and pure-backoff surfaces no longer pay an unconditional image-cache probe in the pending-work summary.
- Added regression coverage for both the one-probe reuse and the conditional/lazy probe shape.

## Why

The prior cache-presence reuse removed duplicate probes but accidentally made the probe unconditional for every non-failed surface. On steady graphics redraws many surfaces are native-animation, already uploaded, or only backing off and do not need image-cache presence for summary classification. Conditional reuse keeps the win while avoiding unnecessary HashMap probes.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/kitty.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_e5a0d1"` — `tj-cafd2704`, passed
