# Session summary — log viewport height scan optimisation

## Goal

Continue the active caco-tui optimiser cycle by measuring the current real-TUI benchmark, selecting one focused hot path in the slowest scene, and reducing CPU/layout work without changing visible log behaviour.

## Bead(s)

- `bd-18f0ed` — Avoid allocating log spans while measuring viewport height.

## Before state

- Failing tests: none known at the start of the slice.
- Relevant metrics: release real-TUI fixture benchmark, text-mode tmux, `default_animated`, 8s measurement after `bd-004e21` on main: paced ~62.50 FPS, work headroom ~317.9 FPS, average work ~3.15ms, median ~3.15ms, p95 ~5.60ms, p99 ~10.61ms. `feed_logs` was the slowest scene at ~276.9 work FPS / ~3.61ms average work / p95 ~6.41ms. `graphics_capability` was `None` and uploads/frame was 0.
- Context: `views/logs.rs` built owned styled spans during the viewport anchor/visual-height scans, then built the same spans again for visible row rendering.

## After state

- Failing tests: none observed in validation below.
- Relevant metrics: after the change, the same benchmark path reported paced ~62.50 FPS, work headroom ~384.2 FPS, average work ~2.60ms, median ~1.77ms, p95 ~6.23ms, p99 ~8.16ms. `feed_logs` improved to ~341.4 work FPS / ~2.93ms average work. `graphics_capability` remained `None` with zero uploads, so this is text-mode CPU/layout headroom evidence rather than kitty upload proof.
- Context: log viewport anchoring now computes visual heights with allocation-free character-width helpers that mirror the rendered span width, including multiselect prefix, timestamp, padded level, source wrapper, and message length.

## Diff summary

- Commits: pending reintegration receipt for final landed SHA.
- Files touched: `crates/caco-tui/src/views/logs.rs`, `.cacophony/profiles/tui-animation-optimiser.md`, `.cacophony/agent/helsinki-cacophony-caco-tui/summary/pending/summary.md`.
- Tests: +2 regression tests for log visual-height width calculation; no tests removed or flipped.
- Behavioural delta: no intended UI behaviour change. The rendered rows still use the existing styled span construction; only pre-render height scans avoid constructing those spans.
- Validation: `cargo test -p caco-tui bd_18f0ed`; `cargo clippy -p caco-tui --lib -- -D warnings`; `cargo test -p caco-tui`; `rustfmt --edition 2021 crates/caco-tui/src/views/logs.rs`; `git diff --check`; before/after `./scripts/tui-fps-bench.sh --release --duration 8 --warmup 2 --debug --extra-config-yaml 'tui.graphics.theme_name: default_animated'`.

## Operator-takeaway

The active optimiser loop found and removed another per-frame allocation source in the real TUI benchmark. The biggest measured win is in the `feed_logs` scene, where average work dropped from about 3.61ms to 2.93ms while preserving the log wrapping/selection model.
