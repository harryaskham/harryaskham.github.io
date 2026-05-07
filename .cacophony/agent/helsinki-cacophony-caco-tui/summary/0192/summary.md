# Session summary — merged log cache render access

## Goal

Continue the active caco-tui optimiser loop by measuring the post-log-height baseline, then removing one remaining per-frame allocation in the logs view while preserving filtered log behaviour.

## Bead(s)

- `bd-288d86` — Avoid per-frame merged log Vec allocation when filters are inactive.

## Before state

- Failing tests: none known at the start of the slice.
- Relevant metrics: release real-TUI fixture benchmark, text-mode tmux, `default_animated`, 8s measurement after `bd-18f0ed` on main: paced ~62.50 FPS, work headroom ~512.7 FPS, average work ~1.95ms, median ~1.27ms, p95 ~5.17ms, p99 ~7.50ms. `feed_logs` was ~458.4 work FPS / ~2.18ms average work / p95 ~5.91ms. `graphics_capability` was `None` and uploads/frame was 0.
- Context: `TuiState::merged_logs()` reused a sorted ref cache but still cloned cached refs and collected a fresh `Vec<&LogEntry>` for every logs render. The no-filter logs view only needed indexed access to that cached ordering.

## After state

- Failing tests: none observed in validation below.
- Relevant metrics: after keeping cache refs local to the render pass and resolving entries from those refs, the same benchmark path reported paced ~62.50 FPS, work headroom ~547.1 FPS, average work ~1.83ms, median ~1.25ms, p95 ~4.54ms, p99 ~6.39ms. `feed_logs` improved to ~731.0 work FPS / ~1.37ms average work / p95 ~2.02ms. `graphics_capability` remained `None` with zero uploads, so this is text-mode CPU/layout headroom evidence rather than kitty upload proof.
- Context: a first attempt using per-index cache accessors regressed because it borrowed/rechecked the cache repeatedly. The final version clones lightweight cached refs once per render and resolves entries through those refs, preserving the existing filtered path.

## Diff summary

- Commits: pending reintegration receipt for final landed SHA.
- Files touched: `crates/caco-tui/src/state/mod.rs`, `crates/caco-tui/src/state/tests.rs`, `crates/caco-tui/src/views/logs.rs`, `.cacophony/profiles/tui-animation-optimiser.md`, `.cacophony/agent/helsinki-cacophony-caco-tui/summary/pending/summary.md`.
- Tests: +1 state regression test (`merged_log_index_access_matches_merged_logs_bd_288d86`); no tests removed or flipped.
- Behavioural delta: no intended UI behaviour change. Filtered log views still use the existing filtered merged-log vector; unfiltered log views avoid building an additional `Vec<&LogEntry>` each frame.
- Validation: `cargo test -p caco-tui bd_288d86`; `cargo clippy -p caco-tui --lib -- -D warnings`; `cargo test -p caco-tui`; `rustfmt --edition 2021 crates/caco-tui/src/views/logs.rs crates/caco-tui/src/state/mod.rs crates/caco-tui/src/state/tests.rs`; `git diff --check`; before/after `./scripts/tui-fps-bench.sh --release --duration 8 --warmup 2 --debug --extra-config-yaml 'tui.graphics.theme_name: default_animated'`.

## Operator-takeaway

The optimiser loop found that the logs view still allocated a merged-log entry vector every frame even after prior span-height work. Keeping lightweight cached refs for the render pass produced a measured `feed_logs` headroom improvement while leaving filtered logs on their existing behaviour path.
