# bd-788ca0: keep TUI background cache hot across border-only state changes

## What changed

- Narrowed `BackgroundSurfaceSnapshot` so it tracks only fields that can affect background rendering:
  - area, role, background fill, background animation bit, instance, background-image override, rotation seed, line height, render scale, and image identities.
- Removed border-only fields from background fast-path invalidation:
  - panel focus/selection state, foreground/border color, title gaps, bottom gaps, border visibility, and nesting level.
- Added regression coverage proving border-only changes keep the same background snapshot while real background fill changes still invalidate it.

## Why

The app-level background cache is the fastest path: it marks existing background surfaces live without re-entering background renderer key construction, hashing, or raster/cache lookup. The previous snapshot stored the full `GraphicsBorderRequest`, so focus changes or title-gap/border-visibility changes that only affect border chrome also invalidated background caching. Keeping backgrounds hot across those changes reduces avoidable graphics work while preserving correctness for actual background changes.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_788ca0"`
  - first attempt hit transient daemon reachability
  - retry `tj-77eee737`, passed
