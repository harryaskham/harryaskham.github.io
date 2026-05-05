# bd-db6142: keep shared retained Kitty aliases zero-byte on repeated redisplays

## What changed

- `SurfaceManager::mark_retained()` now inspects an existing local retained variant's image id and byte size before deciding the accounting size for a retained redisplay.
- A logical surface aliasing a globally shared retained Kitty payload keeps zero-byte accounting not only on first alias creation, but also on repeated redisplays of the same shared image/hash.
- Added regression coverage proving repeated alias redisplays do not replace zero-byte accounting with full payload bytes.

## Why

bd-5f4eb5 made first-time shared retained aliases zero-byte so global retained-byte accounting reflects terminal payload storage instead of logical alias count. Repeated redisplays of the same alias could still see a local variant already present and overwrite its zero byte size with the payload size. That inflated retained bytes, causing premature retained-cache eviction and avoidable full bitmap uploads. This keeps long-running shared border/background payload reuse stable.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/kitty.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_db6142"` — `tj-31ca27a3`, passed
