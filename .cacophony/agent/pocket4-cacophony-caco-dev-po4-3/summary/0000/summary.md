# bd-b9d3a2: collapsible columns in agent detail pane

## Goal
Add per-column collapse/expand toggles to the 3-column agent-detail info section in the TUI so the operator can reclaim screen space when one or two columns aren't relevant for the current task. Acceptance criteria specified: each column has a collapse/expand toggle; collapsed columns take minimal space; state preserved during session; no layout regressions.

## Bead(s)
- bd-b9d3a2 — Make agent detail pane columns collapsible in TUI (P2)
- Background: bd-e9f5a4 (the original 3-column responsive layout)

## Before state
- `crates/caco-tui/src/views/agent_detail.rs::render_agent_info` rendered three fixed-percentage columns (33/34/33 in wide mode, 55/45 in narrow). No way to hide a column to focus on the others — operators with goal-text or in-flight-ops dominating the right column had no way to give it more space.

## After state
- `TuiState` gains `agent_detail_collapsed_cols: [bool; 3]` (default `[false; 3]`). Index 0=left identity, 1=middle environment, 2=right temporal/attach. State is per-session (not persisted across daemon restart) per AC3.
- `Alt+1`, `Alt+2`, `Alt+3` toggle the corresponding column when the current content pane is `AgentDetail` or `PersistentAgentDetail`. Bindings are gated so the digits stay free for other panes.
- New helper `compute_collapsible_constraints([u16; 3], [bool; 3], chip_width)` builds layout constraints: collapsed columns get a fixed `Length(chip_width)` (3 cells); freed percentage is redistributed proportionally across remaining expanded columns. If all three are collapsed each still gets chip width so the operator can re-expand.
- 2-column narrow fallback handles its own constraint shape so the helper never sees a phantom 0% middle column (would have collapsed the right column to 0% width — caught by the existing render-snapshot tests on first cargo test pass).
- New helper `render_collapsed_column_chip(frame, area, digit)` paints a dim NORD3 `│N│` chip in a collapsed column's slot so the operator can see the column is still there and which Alt-digit re-expands it.

## Diff summary
- `crates/caco-tui/src/state/mod.rs` — added `agent_detail_collapsed_cols: [bool; 3]` field + Default initializer.
- `crates/caco-tui/src/app.rs` — added Alt+1/2/3 keybind block gated on AgentDetail/PersistentAgentDetail content pane.
- `crates/caco-tui/src/views/agent_detail.rs` — added `compute_collapsible_constraints`, `render_collapsed_column_chip`, threaded collapse flags through left/middle/right render branches, separated 2-col fallback into its own match-based constraint shape.
- 5 new unit tests in `views::agent_detail::tests` covering: none-collapsed preserves percentages, single-column collapse + redistribution (left/middle separately verified), all-three collapsed all get chip width, custom chip width respected.

## Operator-takeaway
On any agent-detail or persistent-agent-detail view, press `Alt+1` / `Alt+2` / `Alt+3` to collapse the left / middle / right column of the info section. Collapsed columns shrink to a `│1│` / `│2│` / `│3│` chip so you can see what's hidden and how to re-expand. The freed width is redistributed proportionally across the remaining expanded columns. State is per-session — defaults all-expanded on TUI restart, matching AC3 ("preserved during user session") without dragging in cross-session persistence wiring.

## Tests
- 5 new unit tests on `compute_collapsible_constraints` cover all collapse-permutation shapes
- `cargo test -p caco-tui --lib views::agent_detail`: 61 passed (including the existing render-snapshot tests that caught the initial 2-col 0% bug)
- `cargo test -p caco-tui --lib`: 2930 passed
- `cargo test-small`: 190 passed

## AC mapping
1. ✅ Each of the 3 columns has a collapse/expand toggle (Alt+1, Alt+2, Alt+3)
2. ✅ Collapsed columns take minimal space (3-char chip with column digit)
3. ✅ State preserved during user session (per-session TuiState field, defaults expanded)
4. ✅ Doesn't break existing layout (61 agent_detail tests pass, including all pre-existing render-snapshot tests)
