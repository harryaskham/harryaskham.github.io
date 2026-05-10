# Session summary — top-level p99 terminal sync

## Goal

Close the remaining headline terminal-sync tail gap in the real-TUI benchmark JSON by adding top-level p99 terminal-sync timing, matching the per-scene p99 terminal-sync metric that already existed.

## Bead(s)

- `bd-b6ae0d` — Expose top-level p99 terminal-sync timing in TUI benchmark JSON

## Before state

- Failing tests: none known for this slice.
- Relevant metrics: after `bd-d93bdb`, top-level benchmark JSON included combined p95/p99 terminal-inclusive frame time and top-level `avg_terminal_sync_ms` / `p95_terminal_sync_ms`, but not top-level `p99_terminal_sync_ms`; scene summaries already exposed `p99_terminal_sync_ms`.
- Context: quick headline benchmark comparisons still had to inspect scene summaries or raw terminal-sync samples to see high-tail terminal-side processing cost.

## After state

- Failing tests: none observed.
- Relevant metrics: actual Kitty verification at `/tmp/caco-fps-bd-b6ae0d-after.json` reported `graphics_capability=Kitty`, work FPS ≈410.9, terminal-inclusive FPS ≈175.6, `p95_terminal_sync_ms≈3.36`, and new top-level `p99_terminal_sync_ms≈3.49`. The expected `source_dirty` caveat was present because the benchmark binary was rebuilt from the dirty pre-commit tree.
- Context: this is observability-only; rendering, upload, and terminal synchronization behavior are unchanged.

## Diff summary

- Commits: final branch/reintegration commit to be assigned by `caco agent reintegrate`.
- Files touched: `crates/caco-tui/src/app/benchmark_support.rs`, `.cacophony/profiles/tui-animation-optimiser.md`.
- Tests: added a focused assertion for top-level `p99_terminal_sync_ms`; no tests removed.
- Behavioural delta: real-TUI benchmark JSON now includes top-level `p99_terminal_sync_ms` derived from the existing terminal-sync duration summary.
- Validation: `./scripts/rustfmt-changed.sh`; `git diff --check`; queued `cargo check -p caco-tui` (`tj-b93f36b2`); queued focused terminal-sync benchmark test (`tj-9f08f935`); queued `cargo test -p caco-tui app::benchmark_support::tests` (`tj-bf8828e6`); actual Kitty benchmark JSON check (`/tmp/caco-fps-bd-b6ae0d-after.json`); queued `cargo clippy -p caco-tui --lib -- -D warnings` (`tj-195713ee`); queued `cargo test -p caco-tui` (`tj-ac543da0`).

## Operator-takeaway

Headline real-TUI graphics benchmark JSON now shows p99 terminal-sync cost directly, so future optimizer cycles can compare app-frame, upload-pass, terminal-sync, and combined terminal-inclusive tails without manual scene traversal.
