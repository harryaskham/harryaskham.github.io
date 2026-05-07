# Session summary — bead table updated-age timestamp reuse

## Goal

Continue the active caco-tui optimiser loop after the feed relative-time slice by measuring current main, identifying one narrow render hot path, and landing a low-risk optimisation that preserves visible TUI behaviour.

## Bead(s)

- `bd-0f4848` — Reuse bead table render timestamp for updated ages.

## Before state

- Failing tests: none known at the start of the slice.
- Relevant metrics: release real-TUI fixture benchmark, text-mode tmux, `default_animated`, 8s measurement on `f003e6340`: paced ~62.5 FPS, work headroom ~716 FPS, average work ~1.40ms, median ~1.17ms, p95 ~2.26ms. Slowest scenes were `feed_logs` ~1.94ms average work and `project_beads_board` ~1.67ms average work / ~598 work FPS. `graphics_capability` was `None` and uploads/frame was 0.
- Context: no focused TUI/performance bead was assigned, and the only open `performance` bead was daemon/state-sync work assigned to Harry, so this agent self-filed a scoped caco-tui optimisation bead.

## After state

- Failing tests: none observed in validation below.
- Relevant metrics: best follow-up release real-TUI run after the change, under noisier host load, reported paced ~62.5 FPS, work headroom ~540.5 FPS, average work ~1.85ms, median ~1.80ms, p95 ~2.35ms. The target `project_beads_board` scene improved from ~1.67ms / ~598 work FPS to ~1.62ms / ~618 work FPS. Overall benchmark headroom regressed in the noisy run due to other scenes (`feed_logs`, `overview_agents`, `modal_overlay`) getting slower, so the evidence should be read as a narrow bead-table scene win rather than an overall FPS win.
- Context: project and global bead tables now capture one `Utc::now()` per render and compute every visible updated-at cell against it. A shared `common::human_staleness_ago_at(now, ts)` helper also replaces the local feed helper from the previous slice so row/list renderers can reuse the same age-bucket logic.

## Diff summary

- Commits: pending reintegration receipt for final landed SHA.
- Files touched: `crates/caco-tui/src/views/common.rs`, `crates/caco-tui/src/views/beads.rs`, `crates/caco-tui/src/views/global_beads.rs`, `crates/caco-tui/src/views/feed.rs`, `.cacophony/profiles/tui-animation-optimiser.md`, `.cacophony/agent/helsinki-cacophony-caco-tui/summary/pending/summary.md`.
- Tests: +1 deterministic supplied-now staleness helper test; no tests removed or flipped.
- Behavioural delta: no intended UI wording or layout change. Existing relative age buckets (`now`, `42s ago`, `5m ago`, `1mo ago`, etc.) are preserved while rows within a render frame use one consistent clock value.
- Validation: `cargo test -p caco-tui bd_0f4848`; `cargo test -p caco-tui staleness_ago_at_uses_supplied_now_bd_0f4848`; `cargo clippy -p caco-tui --lib -- -D warnings`; `cargo test -p caco-tui`; `git diff --check`; before/after `./scripts/tui-fps-bench.sh --release --duration 8 --warmup 2 --debug --extra-config-yaml 'tui.graphics.theme_name: default_animated'`.

## Operator-takeaway

This slice generalised the successful “single render timestamp” pattern from feed rows into the shared age-formatting helper and bead tables. The measured project bead-board scene improved modestly, but the whole-dashboard benchmark was noisy enough that this should not be counted as an overall FPS advance.
