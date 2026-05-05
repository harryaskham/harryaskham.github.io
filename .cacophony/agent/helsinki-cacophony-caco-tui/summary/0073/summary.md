# bd-5f4eb5: track shared retained Kitty image IDs after TUI redisplays

## What changed

- `SurfaceManager::mark_retained()` now detects when a surface is being attached to an already globally shared retained image payload.
- The alias surface still records the retained image ID locally, preserving placement/delete tracking and retained redisplay lookup behavior.
- The alias no longer adds the same terminal-retained payload bytes to `retained_total_bytes` a second time.
- Added regression coverage for shared retained redisplay aliases: the second surface tracks the shared image ID while global retained bytes remain counted once.

## Why

Different logical surfaces can reuse one identical retained Kitty payload. After a retained redisplay, each surface needs local retained tracking so future placement/deletion behavior is correct, but the terminal stores only one payload. Counting shared aliases as fresh payload bytes would make the global retained-byte budget look larger than real terminal memory, causing premature eviction and avoidable full re-uploads.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/kitty.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_5f4eb5"` — `tj-db5d7bf4`, passed
