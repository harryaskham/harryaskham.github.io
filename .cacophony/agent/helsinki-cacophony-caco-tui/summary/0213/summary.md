# Session summary — ASCII bead-title sort keys

## Goal

Continue the active caco-tui optimiser loop with actual Kitty graphics evidence and remove one narrow allocation in the bead-board sorting path.

## Bead(s)

- `bd-1925d3` — Avoid ASCII bead title lowercase sort allocation.

## Before state

- Failing tests: none known for this slice; clippy remains blocked by unrelated `bd-ae7bc4` caco-config large-enum warnings.
- Relevant metrics: actual Xvfb/kitty real-TUI fixture on main `e789ef867`, `default_animated`, uncapped, terminal-sync enabled, 5s measurement: `graphics_capability=Kitty`, uploads=226, deletes=235, upload wire bytes ≈26.55MB, app-side work FPS ≈439.8, terminal-inclusive work FPS ≈181.4, avg work ≈2.27ms, avg terminal-inclusive ≈5.51ms, avg upload pass ≈0.73ms. Project bead-board scene: ≈373.2 work FPS, avg ≈2.68ms.
- Context: `TuiState::sort_bead_refs()` prepared `title_lower = bead.title.to_lowercase()` for every sorted bead. The benchmark/project-board path mostly sorts ASCII bead titles, so this paid a heap allocation per bead even when a byte-wise ASCII folded comparator is sufficient.

## After state

- Failing tests: none observed in validation below.
- Relevant metrics: actual Xvfb/kitty after-runs remained graphics-gated (`graphics_capability=Kitty`, terminal-sync enabled, uploads=226, deletes=235, upload wire bytes ≈26.55MB). Evidence was noisy/mixed: first after-run was slower at app-side work FPS ≈355.4 / terminal-inclusive ≈163.3, while the immediate rerun improved to app-side work FPS ≈577.4 / terminal-inclusive ≈201.7, avg work ≈1.73ms, avg terminal-inclusive ≈4.96ms, avg upload pass ≈0.59ms, project bead-board ≈534.7 work FPS. Treat this as a targeted allocation cleanup, not a guaranteed broad FPS win.
- Context: `PreparedBeadSort` now stores a lowercase title key only for non-ASCII titles. ASCII titles are compared with an allocation-free folded comparator; mixed/non-ASCII comparisons preserve the prior lowercase ordering path.

## Diff summary

- Commits: pending reintegration receipt for final landed SHA.
- Files touched: `crates/caco-tui/src/state/mod.rs`, `crates/caco-tui/src/state/tests.rs`, `.cacophony/profiles/tui-animation-optimiser.md`, `.cacophony/agent/helsinki-cacophony-caco-tui/summary/pending/summary.md`.
- Tests: added `prepared_bead_title_sort_avoids_ascii_lowercase_allocation_bd_1925d3` to assert ASCII titles avoid lowercase key allocation and Unicode fallback keys remain present.
- Behavioural delta: no intended UI change. Bead title sorting remains case-insensitive for ASCII titles and preserves non-ASCII lowercase fallback behavior.
- Validation: `cargo test -p caco-tui prepared_bead_title_sort_avoids_ascii_lowercase_allocation_bd_1925d3`; `cargo test -p caco-tui sorted_beads_respects_column_sort`; `cargo check -p caco-tui`; `cargo test -p caco-tui`; `git diff --check`; before and after `./scripts/tui-fps-bench.sh --release --graphics --uncapped --duration 5 --warmup 1 --debug --extra-config-yaml 'tui.graphics.theme_name: default_animated'`. Clippy remains blocked by unrelated `bd-ae7bc4`.

## Operator-takeaway

This slice removes a per-bead sort allocation from the common ASCII title path while keeping Unicode fallback behavior intact; the graphics benchmark stayed noisy, so the safe claim is allocation cleanup rather than a deterministic FPS improvement.
