# Session summary — borrowed log source wrappers

## Goal

Continue the active caco-tui optimiser loop with actual Kitty graphics evidence and remove another visible log row allocation from the feed/logs render path.

## Bead(s)

- `bd-d54708` — Borrow log source wrapper spans.

## Before state

- Failing tests: none known for this slice; clippy remains blocked by unrelated `bd-ae7bc4` caco-config large-enum warnings.
- Relevant metrics: actual Xvfb/kitty real-TUI fixture on main `dc91f0f4d`, `default_animated`, uncapped, terminal-sync enabled, 5s measurement: `graphics_capability=Kitty`, uploads=226, deletes=235, upload wire bytes ≈26.55MB, app-side work FPS ≈378.3, terminal-inclusive work FPS ≈171.0, avg work ≈2.64ms, avg terminal-inclusive ≈5.85ms, avg upload pass ≈0.77ms. Feed/logs scene was ≈419.7 work FPS, avg ≈2.38ms.
- Context: after the previous log-row allocation cleanups, `build_log_entry_spans()` still allocated `format!("[{source}] ")` for every visible log row that carried a source.

## After state

- Failing tests: none observed in validation below.
- Relevant metrics: actual Xvfb/kitty after-run stayed graphics-gated (`graphics_capability=Kitty`, terminal-sync enabled, uploads=226, deletes=235, upload wire bytes ≈26.55MB). App-side work FPS ≈479.0, terminal-inclusive work FPS ≈187.3, avg work ≈2.09ms, avg terminal-inclusive ≈5.34ms, avg upload pass ≈0.68ms. Feed/logs scene was roughly neutral/slightly better at ≈421.3 work FPS, avg ≈2.37ms.
- Context: log source rendering now uses borrowed `[` and `] ` spans plus borrowed source text, preserving the exact `[source] ` output and source color while avoiding the formatted wrapper allocation.

## Diff summary

- Commits: pending reintegration receipt for final landed SHA.
- Files touched: `crates/caco-tui/src/views/logs.rs`, `.cacophony/profiles/tui-animation-optimiser.md`, `.cacophony/agent/helsinki-cacophony-caco-tui/summary/pending/summary.md`.
- Tests: added `log_source_spans_borrow_wrapper_text_bd_d54708` to assert the rendered source wrapper is unchanged and its parts are borrowed.
- Behavioural delta: no intended UI change. Log source colors, row width, wrapping, horizontal scrolling, multiselect styling, and search highlighting are preserved.
- Validation: `cargo test -p caco-tui log_source_spans_borrow_wrapper_text_bd_d54708`; `cargo test -p caco-tui log_entry_visual_height_matches_render_span_width_bd_18f0ed`; `cargo test -p caco-tui log_entry_spans_borrow_no_search_row_text_bd_78be74`; `cargo check -p caco-tui`; `cargo test -p caco-tui`; `git diff --check`; before and after `./scripts/tui-fps-bench.sh --release --graphics --uncapped --duration 5 --warmup 1 --debug --extra-config-yaml 'tui.graphics.theme_name: default_animated'`. Clippy remains blocked by unrelated `bd-ae7bc4`.

## Operator-takeaway

This removes the per-row formatted log source wrapper allocation while keeping the exact log row text intact; the overall Kitty benchmark improved, while the feed/logs scene itself was roughly neutral.
