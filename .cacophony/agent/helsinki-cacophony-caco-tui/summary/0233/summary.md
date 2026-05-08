# Session summary — ASCII-fast log row width accounting

## Goal

Continue the active caco-tui optimiser loop with actual Kitty graphics evidence and remove remaining ASCII-heavy character-count scans from log row width accounting and visual-height estimation.

## Bead(s)

- `bd-76b21f` — Fast-path ASCII log row width accounting.

## Before state

- Failing tests: none known for this slice; clippy remains blocked by unrelated `bd-ae7bc4` caco-config large-enum warnings.
- Relevant metrics: actual Xvfb/kitty real-TUI fixture on main `e09fe9693`, `default_animated`, uncapped, terminal-sync enabled, 5s measurement: `graphics_capability=Kitty`, uploads=226, deletes=235, upload wire bytes ≈26.55MB, app-side work FPS ≈397.6, terminal-inclusive work FPS ≈175.0, avg work ≈2.51ms, avg terminal-inclusive ≈5.72ms, avg upload pass ≈0.73ms. `project_beads_board` was ≈308.5 work FPS / avg ≈3.24ms; `feed_logs` was ≈442.7 work FPS / avg ≈2.26ms.
- Context: `views/logs.rs` still used `.chars().count()` for log level padding and log entry visual-height width estimates across timestamp, level, source, and message fields, even though `common::text_width()` now has an ASCII fast path and Unicode fallback.

## After state

- Failing tests: none observed in validation below.
- Relevant metrics: actual Xvfb/kitty after-run stayed graphics-gated (`graphics_capability=Kitty`, terminal-sync enabled, uploads=226, deletes=235, upload wire bytes ≈26.55MB). App-side work FPS ≈498.4, terminal-inclusive work FPS ≈192.1, avg work ≈2.01ms, avg terminal-inclusive ≈5.21ms, avg upload pass ≈0.62ms. `project_beads_board` measured ≈500.1 work FPS / avg ≈2.00ms and `feed_logs` measured ≈408.8 work FPS / avg ≈2.45ms.
- Context: log level padding and visual-height accounting now use `common::text_width()`, preserving Unicode fallback and wrapping/right-alignment behavior. The feed/log scene itself was lower/noisy, but overall app and terminal-inclusive metrics improved.

## Diff summary

- Commits: pending reintegration receipt for final landed SHA.
- Files touched: `crates/caco-tui/src/views/logs.rs`, `.cacophony/profiles/tui-animation-optimiser.md`, `.cacophony/agent/helsinki-cacophony-caco-tui/summary/pending/summary.md`.
- Tests: added `log_entry_width_uses_unicode_fallback_with_ascii_fast_path_bd_76b21f` covering a non-ASCII source/message so Unicode fallback remains character-count based.
- Behavioural delta: no intended UI change. Log row padding, wrapping, and visual-height estimates remain character-width based; ASCII values now avoid Unicode char iteration.
- Validation: `cargo test -p caco-tui log_entry_width_uses_unicode_fallback_with_ascii_fast_path_bd_76b21f`; `cargo test -p caco-tui log_entry_visual_height`; `cargo test -p caco-tui log_level_spans_preserve_width_without_owned_padding_bd_4ca5dd`; `cargo check -p caco-tui`; `cargo test -p caco-tui`; `git diff --check`; before and after `./scripts/tui-fps-bench.sh --release --graphics --uncapped --duration 5 --warmup 1 --debug --extra-config-yaml 'tui.graphics.theme_name: default_animated'`. Clippy remains blocked by unrelated `bd-ae7bc4`.

## Operator-takeaway

Log row width accounting now shares the same ASCII-fast helper as feed and bead tables, removing another repeated hot-path character scan while keeping Unicode-safe log wrapping and right-aligned level padding unchanged.
