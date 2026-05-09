# Session summary — Cache log row theme colors

## Goal

Continue the active caco-tui optimiser loop with actual Kitty graphics evidence and remove repeated log-row theme color lookups while preserving rendered log rows.

## Bead(s)

- `bd-933da4` — Cache log row theme colors.

## Before state

- Failing tests: none known for this slice; clippy remains blocked by unrelated `bd-ae7bc4` caco-config large-enum warnings.
- Relevant metrics: actual Xvfb/kitty real-TUI fixture on main `18f5ebdf7`, `default_animated`, uncapped, terminal-sync enabled, 5s measurement: `graphics_capability=Kitty`, uploads=228, deletes=237, upload wire bytes ≈26.55MB, app-side work FPS ≈299.8, terminal-inclusive work FPS ≈150.5, avg work ≈3.34ms, avg terminal-inclusive ≈6.64ms, avg upload pass ≈1.02ms. `overview_agents` was ≈706.7 work FPS / avg ≈1.42ms, `project_beads_board` was ≈234.0 / avg ≈4.27ms, and `feed_logs` was ≈302.5 / avg ≈3.31ms.
- Context: log row rendering called `common::theme()` repeatedly inside every visible row for timestamp, mark, source, message, search-match, and marked-row styles.

## After state

- Failing tests: none observed in validation below.
- Relevant metrics: actual Xvfb/kitty after-run stayed graphics-gated (`graphics_capability=Kitty`, terminal-sync enabled, uploads=226, deletes=235, upload wire bytes ≈26.55MB). App-side work FPS ≈483.5, terminal-inclusive work FPS ≈189.5, avg work ≈2.07ms, avg terminal-inclusive ≈5.28ms, avg upload pass ≈0.68ms. `project_beads_board` measured ≈442.1 work FPS / avg ≈2.26ms, `feed_logs` ≈427.6 / avg ≈2.34ms, and `overview_agents` ≈758.3 / avg ≈1.32ms.
- Context: log rendering now builds a compact `LogRowTheme` once per render and passes it into log span construction. Per-row level color remains dynamic because it depends on the log level.

## Diff summary

- Commits: pending reintegration receipt for final landed SHA.
- Files touched: `crates/caco-tui/src/views/logs.rs`, `.cacophony/profiles/tui-animation-optimiser.md`, `.cacophony/agent/helsinki-cacophony-caco-tui/summary/pending/summary.md`.
- Tests: updated existing log span helper tests to pass the cached row theme.
- Behavioural delta: no intended UI/layout change; log row styles are preserved while stable theme color lookups are hoisted out of visible row loops.
- Validation: `./scripts/rustfmt-changed.sh`; `cargo test -p caco-tui log_entry_spans_borrow_no_search_row_text_bd_78be74`; `cargo test -p caco-tui log_source_spans_borrow_wrapper_text_bd_d54708`; `cargo check -p caco-tui`; `cargo test -p caco-tui`; `git diff --check`; before and after `./scripts/tui-fps-bench.sh --release --graphics --uncapped --duration 5 --warmup 1 --debug --extra-config-yaml 'tui.graphics.theme_name: default_animated'`. Clippy remains blocked by unrelated `bd-ae7bc4`.

## Operator-takeaway

Log rows now reuse a per-render theme cache instead of re-reading theme colors for every visible log line. Against a noisy low baseline, actual Kitty evidence improved feed/logs, bead-board, overview, app-side, and terminal-inclusive metrics.
