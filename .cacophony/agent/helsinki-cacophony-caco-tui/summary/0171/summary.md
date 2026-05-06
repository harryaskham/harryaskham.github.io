# bd-bf78d4: skip empty background retain GC branches

## What changed

- `BackgroundRenderer::retain_surfaces()` now skips the single-layer shared-cache GC branch when `shared_cache` is empty.
- It also skips the composite shared-cache GC branch when `composite_shared_cache` is empty.
- Per-surface cleanup (`surface_keys`, `composite_surface_keys`, and `surface_image_counters`) still runs before those guards.
- Non-empty shared caches still build referenced key sets, retain live entries, and stash inactive assets for warm reuse.
- Added focused source/runtime coverage for guard placement, counter cleanup with empty caches, and non-empty stale background stashing.

## Why

Retain/GC runs during graphics scene changes and can be reached on frames where one or both background cache families are empty. Previously it still allocated referenced `HashSet`s and rebuilt empty maps via `mem::take`. Guarding empty cache families avoids unnecessary bookkeeping while preserving stale asset retirement and warm inactive-cache reuse.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/background_renderer.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_bf78d4"` — `tj-1d01c92d`, passed
- `caco test run --wait --command "cargo test -p caco-tui retain_surfaces"` — `tj-54f3567a`, passed
