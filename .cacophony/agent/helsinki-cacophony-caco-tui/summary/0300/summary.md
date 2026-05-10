# Session summary — terminal-inclusive frame tails

## Goal

Expose the missing p95/p99 tail metrics for terminal-inclusive real-TUI benchmark frame time so future Kitty/Ghostty graphics optimization can distinguish combined app-work plus terminal-processing stalls from terminal-sync-only or upload-only tails.

## Bead(s)

- `bd-d93bdb` — Expose terminal-inclusive frame tail metrics in TUI benchmark JSON

## Before state

- Failing tests: none known for this slice.
- Relevant metrics: fresh actual Kitty baseline on `0285271a6` reported `graphics_capability=Kitty`, work FPS ≈276.1, terminal-inclusive FPS ≈144.9, average terminal-inclusive frame ≈6.90ms; JSON had only average terminal-inclusive frame cost, while `p95_terminal_inclusive_work_frame_ms` and `p99_terminal_inclusive_work_frame_ms` were absent at top level and per scene.
- Context: the benchmark-observability matrix already exposed cache rates, upload pass tails, terminal-sync tails, byte/delete densities, and graphics activity rates, but combined frame-tail cost still required raw trace reconstruction.

## After state

- Failing tests: none observed.
- Relevant metrics: actual Kitty verification at `/tmp/caco-fps-bd-d93bdb-after.json` reported `graphics_capability=Kitty`, work FPS ≈353.2, terminal-inclusive FPS ≈166.0, top-level `p95_terminal_inclusive_work_frame_ms≈5.59` and `p99_terminal_inclusive_work_frame_ms≈5.72`; per-scene p95/p99 terminal-inclusive frame fields were present for `overview_agents`, `project_beads_board`, and `feed_logs`. The expected `source_dirty` caveat was present because the rebuilt benchmark binary embeds the dirty working tree source hash before the commit.
- Context: this is observability-only; rendering, caching, upload, and terminal-sync behaviour are unchanged.

## Diff summary

- Commits: final branch/reintegration commit to be assigned by `caco agent reintegrate`.
- Files touched: `crates/caco-tui/src/app/benchmark_support.rs`, `.cacophony/profiles/tui-animation-optimiser.md`.
- Tests: updated benchmark-support assertions for combined terminal-inclusive p95/p99 timing; no tests removed.
- Behavioural delta: real-TUI benchmark JSON now includes top-level and per-scene `p95_terminal_inclusive_work_frame_ms` and `p99_terminal_inclusive_work_frame_ms`, computed from each app frame duration plus the corresponding terminal-sync duration.
- Validation: `./scripts/rustfmt-changed.sh`; queued `cargo check -p caco-tui` (`tj-db7a7417`); queued focused benchmark-support test (`tj-4c28b32d`); queued `cargo test -p caco-tui app::benchmark_support::tests` (`tj-5f3aeb8b`); actual Kitty benchmark JSON check (`/tmp/caco-fps-bd-d93bdb-after.json`); queued `cargo clippy -p caco-tui --lib -- -D warnings` (`tj-77d8a292`); queued `cargo test -p caco-tui` (`tj-90cd2b6c`); `git diff --check`.

## Operator-takeaway

The real-TUI benchmark observability matrix now closes the combined terminal-inclusive tail gap: future graphics/cache/upload work can compare app-frame, upload-pass, terminal-sync-only, and full terminal-inclusive p95/p99 costs by scene without rerunning raw traces or guessing from averages.
