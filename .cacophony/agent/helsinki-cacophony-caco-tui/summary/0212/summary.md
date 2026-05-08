# Session summary — allocation-free bead effective section

## Goal

Continue the active caco-tui optimiser loop with actual Kitty graphics evidence and remove a repeated status-normalization allocation from the bead sorting/rendering hot path.

## Bead(s)

- `bd-3d6258` — Avoid bead effective status lowercase allocation.

## Before state

- Failing tests: none known for this slice.
- Relevant metrics: actual Xvfb/kitty real-TUI fixture on main `1f0622ef4`, `default_animated`, uncapped, terminal-sync enabled, 5s measurement: `graphics_capability=Kitty`, uploads=228, deletes=237, upload wire bytes ≈26.55MB, app-side work FPS ≈265.8, terminal-inclusive work FPS ≈141.0, avg work ≈3.76ms, avg terminal-inclusive ≈7.09ms, avg upload pass ≈1.10ms. Project bead-board scene: ≈221.0 work FPS, avg ≈4.53ms.
- Context: `TuiState::bead_effective_section()` called `normalize_state_token(&bead.status)`, allocating a lowercase `String` each time. This helper is hit while preparing sorted bead refs and again while rendering visible rows.

## After state

- Failing tests: none observed in validation below.
- Relevant metrics: actual Xvfb/kitty after run stayed graphics-gated (`graphics_capability=Kitty`, terminal-sync enabled, uploads=226, deletes=235, upload wire bytes ≈26.55MB). App-side work FPS ≈492.8, terminal-inclusive work FPS ≈190.7, avg work ≈2.03ms, avg terminal-inclusive ≈5.24ms, avg upload pass ≈0.63ms. Project bead-board scene: ≈442.5 work FPS, avg ≈2.26ms.
- Context: `bead_effective_section()` now trims once and uses allocation-free `eq_ignore_ascii_case` checks, preserving in-flight open/permanent handling and blocked metadata behavior.

## Diff summary

- Commits: pending reintegration receipt for final landed SHA.
- Files touched: `crates/caco-tui/src/state/mod.rs`, `.cacophony/profiles/tui-animation-optimiser.md`, `.cacophony/agent/helsinki-cacophony-caco-tui/summary/pending/summary.md`.
- Tests: no tests added; existing effective-section tests already cover case/trim normalization plus in-flight behavior.
- Behavioural delta: no intended UI change. Effective bead status/section matching remains case-insensitive but avoids per-call lowercase `String` allocation.
- Validation: `cargo check -p caco-tui`; `cargo test -p caco-tui bead_effective_section_normalizes_status_tokens`; `cargo test -p caco-tui bead_effective_section_spawn_claim_in_flight_is_in_progress`; `cargo test -p caco-tui views::global_beads::tests::render_populated_shows_project_column`; `cargo test -p caco-tui`; `git diff --check`; before/after `./scripts/tui-fps-bench.sh --release --graphics --uncapped --duration 5 --warmup 1 --debug --extra-config-yaml 'tui.graphics.theme_name: default_animated'`. Clippy remains blocked by unrelated `bd-ae7bc4` caco-config large-enum warnings.

## Operator-takeaway

This removes one of the remaining high-frequency allocations in the bead-board state path: effective status checks now borrow the original status string and compare case-insensitively without allocating.
