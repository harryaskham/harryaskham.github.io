# Session summary — Ctrl-P finds per-node status-group views

## Goal

Continue the TUI quick-open improvements by exposing per-node agent status-group panes in Ctrl-P, completing another part of the nav-tree parity gap for machine-group agent inspection.

## Bead(s)

- `bd-dcbbf0` — TUI Ctrl-P should find per-node agent status-group views

## Before state

- Failing tests: none known in the focused TUI lane; no coverage asserted that per-node status-group panes appear in Ctrl-P.
- Relevant metrics: not a performance change.
- Context: Ctrl-P could find global/project aggregate agent status groups and node/machine-group panes, but not per-node status groups such as a specific project/node Failed Agents pane.

## After state

- Failing tests: none in targeted validation.
- Relevant metrics: not applicable.
- Context: Ctrl-P now derives per-node status-group destinations from current agent state and exposes entries like `<project> <node> Failed Agents` that open `StatusGroupView`.

## Diff summary

- Commits: `b35792772`
- Files touched: `crates/caco-tui/src/views/fuzzy_picker.rs`
- Tests: +1 focused per-node status-group test / -0 / existing fuzzy-picker tests preserved
- Behavioural delta: Ctrl-P can now jump directly to machine-specific agent status-group panes using searches such as `node-a failed agents`.
- Validation:
  - `CARGO_BUILD_JOBS=2 cargo test -j2 -p caco-tui fuzzy_picker --lib`

## Operator-takeaway

Ctrl-P now covers node-specific status slices as well as aggregate agent views, making keyboard navigation for machine-level agent triage more complete.
