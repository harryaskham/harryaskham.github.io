# bd-5e9a7e: skip live delete batch allocation when cleanup is empty

## What changed

- `App::flush_graphics_requests()` now checks `!pending_animation_stops.is_empty() || delete_count > 0` before assembling the batched delete command buffer.
- Upload-only graphics frames in batched mode no longer allocate/probe an empty `delete_batch` just to discover there is no cleanup work.
- Non-empty animation-stop, placement-delete, and full-image delete fallback handling remains unchanged.
- Added focused source coverage that asserts the guard precedes `delete_batch` allocation and that fallback delete loops remain present.

## Why

Graphics upload passes can run for retained/full placements with no cleanup queued. The live path had already drained the queues and computed `delete_count`, but still created an empty delete batch on every batched upload frame. Skipping that assembly is another small hot-path reduction toward making retained/cached Kitty frames closer to ASCII/text overhead while preserving correct de-draw behavior when cleanup exists.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_5e9a7e"` — `tj-22ee7350`, passed
- `caco test run --wait --command "cargo test -p caco-tui kitty_upload_pass"` — `tj-7c1486c0`, passed
