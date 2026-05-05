# bd-57b399: demand-driven Kitty upload pass

## What changed

- Tightened `should_run_kitty_upload_pass()` so graphics-capable terminals no longer do a post-draw upload/fetch/delete scan every frame just because Kitty is available.
- The upload pass now runs only when real work is pending:
  - daemon image fetch candidates
  - uncached enhancement uploads
  - native-animation uploads/retries
  - retry backoff state
  - placement/image deletes or animation stops
- The real-dashboard benchmark upload path now uses the same demand gate, so uncapped benchmark evidence reflects the optimized live behavior instead of continuing to measure an obsolete always-scan path.
- Added `SurfaceManager::has_pending_fetch_candidates()` for a cheap fetch preflight without allocating/scanning the full pending list.
- Updated `SPEC.md` to require demand-driven upload passes.

## Evidence

Validation:

- `rustfmt --edition 2021 --check crates/caco-tui/src/app.rs crates/caco-tui/src/app/benchmark_support.rs crates/caco-tui/src/kitty.rs`
- `git diff --check`
- `caco test run --wait --command "cargo test -p caco-tui bd_57b399"` — `tj-dbde3b90`, passed
- `caco test run --wait --command "cargo test -p caco-tui kitty_upload_pass_"` — `tj-8246b542`, passed
- `caco build run --wait --command "cargo build -p caco"` — `bj-43e3138e`, succeeded

Headless Xvfb + Kitty uncapped dev-binary sample after the change:

- `work_fps=104.0`, `avg_work_frame_ms=9.61`
- `frames_with_graphics=2` over 242 frames, showing idle frames now skip graphics upload work
- `uploads_succeeded=108`, `deletes_sent=122`, `upload_wire_bytes=1,682,378`
- `avg_upload_pass_ms=0.208`, `p95_upload_pass_ms=0.031`

This is an incremental performance/correctness fix: it does not remove required initial uploads or stale deletes, but it eliminates unnecessary per-frame upload scans and flush bookkeeping once the surface set is stable.
