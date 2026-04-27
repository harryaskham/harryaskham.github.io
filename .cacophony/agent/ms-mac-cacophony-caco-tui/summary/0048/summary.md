# Session summary — Persistent agent detail honors compact collapse

## Goal

Align persistent agent detail with the new agent-detail compact collapse behavior because the key handler already allowed the collapse keys on persistent-agent panes.

## Bead(s)

- `bd-4e72ec` — TUI persistent agent detail should honor collapse keys
- Related: `bd-71f40c` — TUI agent detail top summary panels should be collapsible

## Before state

- Failing tests: none in the focused lane.
- Relevant metrics: not a performance change.
- Context: `Alt+0` and `Alt+1/2/3` were accepted on both AgentDetail and PersistentAgentDetail panes, but the persistent render path always reserved an 11-row info block and ignored the collapsed state.

## After state

- Failing tests: none in targeted validation.
- Relevant metrics: persistent agent info height now uses the same compact 3-row height when all summary columns are collapsed.
- Context: persistent detail all-collapsed state now renders a compact summary with ID, Name, State, Node, Project, and the same expansion hint, hiding verbose error/recovery metadata until expanded.

## Diff summary

- Commits: `cb6c935e5`
- Files touched: `crates/caco-tui/src/views/agent_detail.rs`
- Tests: +1 focused persistent compact-summary render test / -0
- Behavioural delta: PersistentAgentDetail now honors the all-collapsed summary state instead of wasting the full header height.
- Validation:
  - `CARGO_BUILD_JOBS=2 cargo test -j2 -p caco-tui render_persistent_all_columns_collapsed_shows_compact_summary --lib`

## Operator-takeaway

The compact-collapse behavior now applies consistently to both regular agent detail and persistent agent detail panes.
