# Session summary — ASCII-fast graphics gap widths

## Goal

Continue the active caco-tui optimiser loop with actual Kitty graphics evidence and remove repeated Unicode character-count scans from shared graphics title/bottom gap helpers used while placing native terminal graphics borders.

## Bead(s)

- `bd-5373ea` — Fast-path ASCII graphics gap widths.

## Before state

- Failing tests: none known for this slice; clippy remains blocked by unrelated `bd-ae7bc4` caco-config large-enum warnings.
- Relevant metrics: actual Xvfb/kitty real-TUI fixture on main `407be7616`, `default_animated`, uncapped, terminal-sync enabled, 5s measurement: `graphics_capability=Kitty`, uploads=226, deletes=235, upload wire bytes ≈26.55MB, app-side work FPS ≈345.9, terminal-inclusive work FPS ≈164.5, avg work ≈2.89ms, avg terminal-inclusive ≈6.08ms, avg upload pass ≈0.84ms. `overview_agents` was ≈662.5 work FPS / avg ≈1.51ms; `project_beads_board` was ≈274.6 work FPS / avg ≈3.64ms; `feed_logs` was ≈346.9 work FPS / avg ≈2.88ms.
- Context: `title_gap_aligned()`, `title_gap_from_text()`, `title_gap_with_padding()`, and `bottom_gap_right()` still used `.chars().count()` for ASCII-heavy panel titles and labels every frame.

## After state

- Failing tests: none observed in validation below.
- Relevant metrics: actual Xvfb/kitty after-run stayed graphics-gated (`graphics_capability=Kitty`, terminal-sync enabled, uploads=226, deletes=235, upload wire bytes ≈26.55MB). App-side work FPS ≈488.3, terminal-inclusive work FPS ≈190.1, avg work ≈2.05ms, avg terminal-inclusive ≈5.26ms, avg upload pass ≈0.65ms. `overview_agents` measured ≈649.5 work FPS / avg ≈1.54ms, `project_beads_board` ≈499.0 / avg ≈2.00ms, and `feed_logs` ≈416.8 / avg ≈2.40ms.
- Context: graphics title and bottom gap width helpers now use `text_width()` / `text_width_capped()`, preserving Unicode fallback and visible gap placement. Overview was noisy/lower while bead/log scenes improved.

## Diff summary

- Commits: pending reintegration receipt for final landed SHA.
- Files touched: `crates/caco-tui/src/views/common.rs`, `.cacophony/profiles/tui-animation-optimiser.md`, `.cacophony/agent/helsinki-cacophony-caco-tui/summary/pending/summary.md`.
- Tests: added `graphics_gap_widths_use_ascii_fast_path_with_unicode_fallback_bd_5373ea` covering ASCII titles and non-ASCII labels/titles.
- Behavioural delta: no intended UI change. Graphics gap widths stay character-count based; ASCII titles/labels now avoid Unicode char iteration.
- Validation: `cargo test -p caco-tui graphics_gap_widths_use_ascii_fast_path_with_unicode_fallback_bd_5373ea`; `cargo test -p caco-tui bottom_gap_right`; `cargo check -p caco-tui`; `cargo test -p caco-tui`; `git diff --check`; before and after `./scripts/tui-fps-bench.sh --release --graphics --uncapped --duration 5 --warmup 1 --debug --extra-config-yaml 'tui.graphics.theme_name: default_animated'`. Clippy remains blocked by unrelated `bd-ae7bc4`.

## Operator-takeaway

Graphics gap placement now shares the same ASCII-fast text-width path as feed, logs, and bead tables, trimming another common per-frame cost without changing native-border title or bottom-label placement.
