# Session summary — Fast-path unwrapped log rows

## Goal

Continue the active caco-tui optimiser loop with actual Kitty graphics evidence and remove a little more overhead from the log wrapping path by skipping span-by-span reconstruction when a whole log row already fits.

## Bead(s)

- `bd-2cd1c6` — Fast-path unwrapped log span rows.

## Before state

- Failing tests: none known for this slice; clippy remains blocked by unrelated `bd-ae7bc4` caco-config large-enum warnings.
- Relevant metrics: actual Xvfb/kitty real-TUI fixture on main `57a8807aa`, `default_animated`, uncapped, terminal-sync enabled, 5s measurement: `graphics_capability=Kitty`, uploads=226, deletes=235, upload wire bytes ≈26.55MB, app-side work FPS ≈383.0, terminal-inclusive work FPS ≈171.3, avg work ≈2.61ms, avg terminal-inclusive ≈5.84ms, avg upload pass ≈0.78ms. `overview_agents` was ≈671.7 work FPS / avg ≈1.49ms, `project_beads_board` was ≈311.8 / avg ≈3.21ms, and `feed_logs` was ≈388.5 / avg ≈2.57ms.
- Context: after `bd-de0c4a`, `logs::wrap_spans()` avoided owned chunks for borrowed spans, but still pushed every span through a fresh line vector when the total row width already fit the panel.

## After state

- Failing tests: none observed in validation below.
- Relevant metrics: actual Xvfb/kitty after-run stayed graphics-gated (`graphics_capability=Kitty`, terminal-sync enabled, uploads=226, deletes=235, upload wire bytes ≈26.55MB). App-side work FPS ≈545.0, terminal-inclusive work FPS ≈197.7, avg work ≈1.83ms, avg terminal-inclusive ≈5.06ms, avg upload pass ≈0.60ms. `overview_agents` measured ≈922.2 work FPS / avg ≈1.08ms, `project_beads_board` ≈510.4 / avg ≈1.96ms, and `feed_logs` ≈457.7 / avg ≈2.18ms.
- Context: `wrap_spans()` now computes total row width and, when it fits `max_width`, returns one `Line` from `spans.to_vec()` directly. Wrapping behaviour for over-width rows is unchanged.

## Diff summary

- Commits: pending reintegration receipt for final landed SHA.
- Files touched: `crates/caco-tui/src/views/logs.rs`, `.cacophony/profiles/tui-animation-optimiser.md`, `.cacophony/agent/helsinki-cacophony-caco-tui/summary/pending/summary.md`.
- Tests: +1 log wrapping test for the unwrapped-row fast path.
- Behavioural delta: no intended UI change. Log wrapping preserves existing output; rows that already fit skip the per-span reconstruction loop.
- Validation: `./scripts/rustfmt-changed.sh`; `cargo test -p caco-tui wrap_spans`; `cargo check -p caco-tui`; `cargo test -p caco-tui log_entry_visual_height`; `cargo test -p caco-tui wrap_spans`; `cargo test -p caco-tui`; `git diff --check`; before and after `./scripts/tui-fps-bench.sh --release --graphics --uncapped --duration 5 --warmup 1 --debug --extra-config-yaml 'tui.graphics.theme_name: default_animated'`. Clippy remains blocked by unrelated `bd-ae7bc4`.

## Operator-takeaway

The log wrapper now exits early for already-fitting rows, reducing log render overhead without changing wrapping semantics. The baseline was noisy and low, but actual Kitty evidence improved terminal-inclusive, bead-board, and feed/log measurements for this slice.
