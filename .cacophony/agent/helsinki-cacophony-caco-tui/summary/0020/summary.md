# bd-b9004e: collapse TUI graphics pending-work scans

## What changed

- Added `SurfaceManager::has_pending_graphics_work_candidates()`.
- Updated `should_run_kitty_upload_pass()` to use the combined predicate instead of chaining separate fetch/upload/native-animation preflight helpers.
- Preserved explicit cleanup/delete behavior outside the graphics-capable gate, so stale placement deletes still drain when config disables new uploads but transport can send cleanup commands.
- Added regression coverage that the app upload-pass gate uses the combined predicate and does not chain separate full-surface scans.

## Why

After previous demand-gating work, steady cached graphics frames still paid multiple surface-map preflight scans before deciding that no kitty upload/fetch/native-animation work was pending. Collapsing those checks into one scan trims the common graphics no-op path while preserving upload, fetch, native-animation, and backoff semantics.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs crates/caco-tui/src/kitty.rs`
- `git diff --check`
- First queued validation `tj-e97e1c0c` hit retryable `daemon_restart_recovered` infrastructure.
- `caco test run --wait --command "cargo test -p caco-tui bd_b9004e"` — `tj-b9da71ab`, passed
