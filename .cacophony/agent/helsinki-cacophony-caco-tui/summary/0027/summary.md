# bd-9f3931: keep title decorations live on border animation phase-cache fast path

## What changed

- Extracted title/header decoration lifecycle handling into `ensure_title_decoration_surface()`.
- The unchanged-panel fast path and the completed border phase-cache animation fast path now mark existing title decoration surfaces live and count the reuse as a decoration cache hit.
- The full registration path still renders/registers decorations when needed and retires them when a title gap/style disappears.
- Added regression coverage that completes a two-phase animated border cache, enters the phase-cache fast path, runs the stale-surface sweep, and verifies the unchanged title decoration is not de-drawn.

## Why

The phase-cache path correctly refreshes animated border segment PNGs without renderer lookups, but it returned before the title/header decoration surface was touched. The final redraw stale-surface sweep could therefore retire a valid decoration during animated border redraws. This keeps cached animated borders fast while preserving visible header decoration correctness.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/border_integration.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_9f3931"` — `tj-16441e64`, passed
