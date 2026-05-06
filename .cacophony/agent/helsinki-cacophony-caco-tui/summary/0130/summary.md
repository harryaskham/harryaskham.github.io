# bd-352dab: skip empty background root formatting

## What changed

- `App::flush_graphics_requests()` now checks whether resolved background layers can render a background surface before formatting the `enh:background:<panel>` root key.
- The background cache/lookup/render block runs only when either:
  - the layers contain a renderable background style, or
  - background cache state already exists and may need active-root tracking/pruning/removal.
- Panels whose effective background layers are all `None` and whose cache is empty skip root-key formatting, snapshot lookup, and render dispatch entirely.
- Added source-shape coverage ensuring root formatting stays behind the renderability/cache guard.

## Why

Graphics border requests are common even when a panel has no graphics background. The prior flush path still formatted a background root key and performed background lookup work before `render_graphics_background_with_layers()` eventually returned `None`. This trims empty-background per-panel overhead while preserving stale cache cleanup when prior background cache entries exist.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_352dab"` — `tj-a790efb8`, passed
