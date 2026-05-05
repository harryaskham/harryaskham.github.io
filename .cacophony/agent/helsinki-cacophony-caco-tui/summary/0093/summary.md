# bd-6a1d80: bound background GC fast-path complexity

## What changed

- `BackgroundRenderer::retain_surfaces_gc_not_needed()` now declines the no-GC fast path when active single-layer or composite shared caches contain more than one distinct entry.
- Dense unique-background layouts therefore fall back to the existing HashSet-based retain/GC path instead of doing nested `cache entries × surface mappings` scans.
- Kept the fast path for the common empty/single shared-cache case where the scan is bounded and avoids rebuilding maps on steady frames.
- Added regression coverage for the multi-entry unique-background case.

## Why

The previous no-GC fast path avoided allocations on steady frames, but it checked each active shared cache key with `surface_keys.values().any(...)`. That is fine for one shared asset but can become quadratic when many panels have unique backgrounds. Bounding the fast path keeps the optimization from regressing dense dashboards while preserving the simple steady-frame win.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/background_renderer.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_6a1d80"` — `tj-e9e23155`, passed
