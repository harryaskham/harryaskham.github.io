# bd-bab1cf: avoid TUI batch upload key clone

## What changed

- In the batched Kitty upload path, the full-upload branch now moves the owned surface key into `batch_keys` instead of cloning it.
- Retained-display batch entries already moved the key; this aligns the full-upload branch with that behavior.
- Added regression coverage that the batched full-upload branch no longer contains `key.clone()` for batch accounting.

## Why

Each full bitmap upload already owns the `key` from `pending_uploads()`. After reserving an image ID and building the placement command, the loop no longer needs the original binding except for later success/failure accounting through `batch_keys`. Moving the key avoids a `String` clone per full upload while preserving retained-display behavior, failure handling, and upload counters.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_bab1cf"` — `tj-73a438f6`, passed
