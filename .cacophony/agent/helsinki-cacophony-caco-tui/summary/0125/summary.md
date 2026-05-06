# bd-7030a2: avoid title decoration retire miss on border fast path

## What changed

- Added a guarded title-decoration reuse helper for `BorderIntegration::register_panel()` fast paths.
- Unchanged-panel and phase-cache fast paths now call the guarded helper instead of unconditionally calling `ensure_title_decoration_surface(..., reuse_existing=true)`.
- The guarded helper runs title-decoration work only when either:
  - the current panel/style actually requires a title-decoration surface, or
  - a decoration surface currently exists and may need to be marked live or retired.
- Added source-shape coverage to keep both border fast paths on the guarded helper.

## Why

Most panels do not have title gaps or header decorations. On unchanged/animation-cache fast paths, the old code still called the title-decoration helper, which immediately called `SurfaceManager::retire()` for a title-decoration key that almost always did not exist. That is a guaranteed hash-map miss on every stable graphics frame for every undecorated panel. The new guard avoids that miss while preserving correctness when a decoration is required or when an old decoration must be retired.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/border_integration.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_7030a2"` — `tj-760690da`, passed
