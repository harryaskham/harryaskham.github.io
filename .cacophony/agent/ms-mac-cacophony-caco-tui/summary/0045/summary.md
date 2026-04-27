# Session summary — Compact collapsed agent-detail header

## Goal

Reimplement the intended TUI agent-detail summary collapse so the top metadata panels recover vertical space while still showing the salient fields Harry called out.

## Bead(s)

- `bd-71f40c` — TUI agent detail top summary panels should be collapsible
- Related historical bead: `bd-b9d3a2` — Make agent detail pane columns collapsible in TUI

## Before state

- Failing tests: none in the focused lane.
- Relevant metrics: not a performance change.
- Context: closed `bd-b9d3a2` added Alt+1/Alt+2/Alt+3 per-column collapse, but collapsed columns rendered as 3-character chips and the agent-detail header still reserved 11 rows, so it did not recover vertical space. Harry clarified the collapsed state should still show ID, name, state, node, and diff stats; everything else can hide behind expansion.

## After state

- Failing tests: none in targeted validation.
- Relevant metrics: agent info section height is 3 rows when all three top columns are collapsed, otherwise 11 rows.
- Context: Alt+0 now toggles all three top columns together. When all are collapsed, the header renders a compact salient summary with ID, Name, State, Node, Diff, and an expansion hint, hiding verbose goal/checkout/attach metadata.

## Diff summary

- Commits: `908003357`
- Files touched: `crates/caco-tui/src/views/agent_detail.rs`, `crates/caco-tui/src/app.rs`
- Tests: +2 focused collapse tests / -0
- Behavioural delta: collapsing all agent-detail summary columns now actually gives vertical space back to the tab/terminal area while retaining the key identity/status/diff fields.
- Validation:
  - `CARGO_BUILD_JOBS=2 cargo test -j2 -p caco-tui collapsed --lib`

## Operator-takeaway

The prior collapse implementation existed but only hid column content; this slice makes full collapse useful by shrinking the header and preserving the fields operators still need at a glance.
