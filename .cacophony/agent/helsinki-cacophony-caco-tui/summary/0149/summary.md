# bd-7b158c: avoid background cache lookup closure allocation

## What changed

- Replaced the `Option::then(|| background_cache_lookup_for_layers(...))` wrapper in `App::flush_graphics_requests()` with an explicit `if self.graphics_background_cache.is_empty() { None } else { Some(...) }` branch.
- Cache-empty renderable background frames now skip closure construction while preserving the existing no-lookup behavior.
- Non-empty cache frames still run `background_cache_lookup_for_layers()` and use the same cache-hit/cache-miss behavior.
- Updated source-shape coverage for the explicit branch and the existing empty-cache skip contract.

## Why

The graphics background miss path is a hot path for panels with renderable backgrounds. Using `Option::then(|| ...)` is concise but still builds a closure around the lookup expression. An explicit branch avoids that small per-panel setup on cache-empty frames while keeping behavior unchanged.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_7b158c"` — `tj-7d6fe166`, passed
