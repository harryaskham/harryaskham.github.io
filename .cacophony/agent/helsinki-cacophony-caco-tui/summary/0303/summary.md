# Session summary — headline upload-pass max/slow metrics

## Goal

Expose headline max and slow-frame upload-pass metrics in real-TUI benchmark JSON so future Kitty/Ghostty optimizer runs can identify one-off upload spikes versus repeated interactive-budget upload stalls without walking scene summaries.

## Bead(s)

- `bd-284500` — Expose headline max and slow upload-pass metrics in TUI benchmark JSON

## Before state

- Failing tests: none known for this slice.
- Relevant metrics: top-level benchmark JSON already exposed `avg_upload_pass_ms`, `p95_upload_pass_ms`, and `p99_upload_pass_ms`, while per-scene summaries exposed `max_upload_pass_ms` and `upload_pass_slow_frames`. The headline result still lacked max upload-pass and count of upload passes at or above the 16ms budget.
- Context: after recent benchmark-observability slices, terminal-side headline tails were complete, but headline upload-pass diagnostics still required scene traversal for max/slow counts.

## After state

- Failing tests: none observed.
- Relevant metrics: actual Kitty verification at `/tmp/caco-fps-bd-284500-after.json` reported `graphics_capability=Kitty`, work FPS ≈295.8, terminal-inclusive FPS ≈151.2, `avg_upload_pass_ms≈0.98`, `p95_upload_pass_ms≈0.006`, `p99_upload_pass_ms≈0.008`, new `max_upload_pass_ms≈336.86`, and new `upload_pass_slow_frames=2`. The expected `source_dirty` caveat was present because the benchmark binary was rebuilt from the dirty pre-commit tree.
- Context: this is observability-only; rendering, upload, and terminal synchronization behavior are unchanged.

## Diff summary

- Commits: final branch/reintegration commit to be assigned by `caco agent reintegrate`.
- Files touched: `crates/caco-tui/src/app/benchmark_support.rs`, `.cacophony/profiles/tui-animation-optimiser.md`.
- Tests: extended a focused benchmark-support test to assert headline upload-pass average/p95/p99/max and slow-frame count; no tests removed.
- Behavioural delta: real-TUI benchmark JSON now includes top-level `max_upload_pass_ms` and `upload_pass_slow_frames`, derived from the existing upload-pass duration samples with the same 16ms slow-frame threshold used by scene summaries.
- Validation: `./scripts/rustfmt-changed.sh`; `git diff --check`; queued `cargo check -p caco-tui` (`tj-9fcb80dd`); queued focused upload benchmark test (`tj-4d828ab2`); queued `cargo test -p caco-tui app::benchmark_support::tests` (`tj-5e18f604`); actual Kitty benchmark JSON check (`/tmp/caco-fps-bd-284500-after.json`); queued `cargo clippy -p caco-tui --lib -- -D warnings` (`tj-a8f056d6`); queued `cargo test -p caco-tui` (`tj-1457891c`).

## Operator-takeaway

Headline real-TUI graphics benchmark JSON now shows max upload-pass time and how many upload passes crossed the 16ms budget, making upload-path stall triage possible from the top-level result instead of scene-level/manual trace analysis.
