# Session summary — Cache log row base styles

## Goal

Continue the active caco-tui optimiser loop with actual Kitty graphics evidence and reduce remaining per-row style construction in the feed/logs log-row hot path.

## Bead(s)

- `bd-027ffd` — Cache log row base styles.

## Before state

- Failing tests: none known for this slice; clippy remains blocked by unrelated `bd-ae7bc4` caco-config large-enum warnings.
- Relevant metrics: actual Xvfb/kitty real-TUI fixture on main `f8c88f4a7`, `default_animated`, uncapped, terminal-sync enabled, 5s measurement: `graphics_capability=Kitty`, uploads=226, deletes=235, upload wire bytes ≈26.55MB, app-side work FPS ≈402.7, terminal-inclusive work FPS ≈173.3, avg work ≈2.48ms, avg terminal-inclusive ≈5.77ms, avg upload pass ≈0.72ms. `overview_agents` was ≈744.6 work FPS / avg ≈1.34ms, `project_beads_board` was ≈332.9 / avg ≈3.00ms, and `feed_logs` was ≈393.5 / avg ≈2.54ms.
- Context: after log-level style caching, `build_log_entry_spans()` still rebuilt mark, timestamp, source, message, search-match, and plain separator styles for every visible log row.

## After state

- Failing tests: none observed in validation below.
- Relevant metrics: actual Xvfb/kitty after-run stayed graphics-gated (`graphics_capability=Kitty`, terminal-sync enabled, uploads=226, deletes=235, upload wire bytes ≈26.55MB). App-side work FPS ≈561.7, terminal-inclusive work FPS ≈196.5, avg work ≈1.78ms, avg terminal-inclusive ≈5.09ms, avg upload pass ≈0.62ms. `feed_logs` measured ≈461.1 work FPS / avg ≈2.17ms, `project_beads_board` ≈544.3 / avg ≈1.84ms, and `overview_agents` ≈939.4 / avg ≈1.06ms. The after-run emitted the usual dirty-source warning because the benchmark embeds the last git commit, but it rebuilt without `--no-build`.
- Context: `LogRowTheme` now caches base row styles for plain separators, timestamp variants, mark variants, source variants, message text, and search match/current match spans.

## Diff summary

- Commits: pending reintegration receipt for final landed SHA.
- Files touched: `crates/caco-tui/src/views/logs.rs`, `.cacophony/profiles/tui-animation-optimiser.md`, `.cacophony/agent/helsinki-cacophony-caco-tui/summary/pending/summary.md`.
- Tests: existing log row/source/level compatibility tests passed; no visible-output contract changed.
- Behavioural delta: no intended UI/layout change; base log row styles are hoisted into the per-render log row theme while borrowed no-search message spans and search highlighting remain intact.
- Validation: `./scripts/rustfmt-changed.sh`; `cargo test -p caco-tui log_entry_spans_borrow_no_search_row_text_bd_78be74`; `cargo test -p caco-tui log_source_spans_borrow_wrapper_text_bd_d54708`; `cargo test -p caco-tui log_row_theme_level_styles_match_color_helper_bd_8066a7`; `cargo check -p caco-tui`; `cargo test -p caco-tui views::logs::tests`; `cargo test -p caco-tui`; `git diff --check`; before and after `./scripts/tui-fps-bench.sh --release --graphics --uncapped --duration 5 --warmup 1 --debug --extra-config-yaml 'tui.graphics.theme_name: default_animated'`. Clippy remains blocked by unrelated `bd-ae7bc4`.

## Operator-takeaway

Log rows now reuse cached base styles as well as cached level styles. Actual Kitty evidence improved feed/logs, bead-board, overview, app-side, and terminal-inclusive metrics in this run.
