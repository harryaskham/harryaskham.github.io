# bd-f0fdbb: report retained-only TUI Kitty wire bytes in benchmark telemetry

## What changed

- Added `GraphicsPerfTracker::record_upload_wire_bytes()` for Kitty graphics wire bytes that are not full bitmap uploads.
- Live upload telemetry records retained-only display command wire bytes without incrementing full upload counts or payload bytes.
- Real-dashboard benchmark upload telemetry does the same.
- Updated retained-display regression coverage to assert:
  - retained redisplays do not count as full upload successes,
  - retained redisplays do not count as payload bytes,
  - retained redisplays still report non-zero Kitty wire bytes.
- Added direct perf tracker coverage for retained-only wire-byte accounting.

## Why

After bd-46ac44, retained Kitty redisplays were correctly separated from full bitmap uploads. However, top-level `upload_wire_bytes` was still only recorded through `record_uploads()`, so retained-only frames could show `retained_redisplays > 0` with `upload_wire_bytes = 0` even though `a=p` display commands were sent. This keeps benchmark/live telemetry truthful without reclassifying retained redisplays as full uploads.

## Validation

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs crates/caco-tui/src/app/benchmark_support.rs crates/caco-tui/src/perf.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_f0fdbb"` — `tj-88d2fe7f`, passed
