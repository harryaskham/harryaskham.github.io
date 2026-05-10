# Session summary — headline terminal-sync max/slow metrics

## Goal

Expose headline max and slow-frame terminal-sync metrics in real-TUI benchmark JSON so future Kitty/Ghostty optimizer runs can distinguish one-off terminal-processing spikes from repeated interactive-budget stalls without walking scene summaries.

## Bead(s)

- `bd-b9b563` — Expose headline max and slow terminal-sync metrics in TUI benchmark JSON

## Before state

- Failing tests: none known for this slice.
- Relevant metrics: top-level benchmark JSON already exposed `avg_terminal_sync_ms`, `p95_terminal_sync_ms`, and `p99_terminal_sync_ms`, while per-scene summaries exposed `max_terminal_sync_ms` and `terminal_sync_slow_frames`. The headline result still lacked max terminal-sync and count of sync frames at or above the 16ms budget.
- Context: after recent benchmark-observability slices, terminal-side tail diagnostics were almost complete, but quick run-to-run comparisons still needed scene traversal to identify extreme sync spikes or repeated slow terminal-sync frames.

## After state

- Failing tests: none observed.
- Relevant metrics: actual Kitty verification at `/tmp/caco-fps-bd-b9b563-after.json` reported `graphics_capability=Kitty`, work FPS ≈397.1, terminal-inclusive FPS ≈172.8, `avg_terminal_sync_ms≈3.27`, `p95_terminal_sync_ms≈3.27`, `p99_terminal_sync_ms≈5.28`, new `max_terminal_sync_ms≈27.32`, and new `terminal_sync_slow_frames=2`. The expected `source_dirty` caveat was present because the benchmark binary was rebuilt from the dirty pre-commit tree.
- Context: this is observability-only; rendering, upload, and terminal synchronization behavior are unchanged.

## Diff summary

- Commits: final branch/reintegration commit to be assigned by `caco agent reintegrate`.
- Files touched: `crates/caco-tui/src/app/benchmark_support.rs`, `.cacophony/profiles/tui-animation-optimiser.md`.
- Tests: extended the focused terminal-sync metrics test to assert max sync timing and slow-frame count; no tests removed.
- Behavioural delta: real-TUI benchmark JSON now includes top-level `max_terminal_sync_ms` and `terminal_sync_slow_frames`, derived from the existing terminal-sync duration samples with the same 16ms slow-frame threshold used by scene summaries.
- Validation: `./scripts/rustfmt-changed.sh`; `git diff --check`; queued `cargo check -p caco-tui` (`tj-1174ba94`); queued focused terminal-sync benchmark test (`tj-2f976ca0`); queued `cargo test -p caco-tui app::benchmark_support::tests` (`tj-998ffc60`); actual Kitty benchmark JSON check (`/tmp/caco-fps-bd-b9b563-after.json`); queued `cargo clippy -p caco-tui --lib -- -D warnings` (`tj-95173816`); queued `cargo test -p caco-tui` (`tj-5315ffd2`).

## Operator-takeaway

Headline real-TUI graphics benchmark JSON now shows max terminal-sync time and how many sync frames crossed the 16ms budget, making terminal-side stall triage possible from the top-level result instead of scene-level/manual trace analysis.
