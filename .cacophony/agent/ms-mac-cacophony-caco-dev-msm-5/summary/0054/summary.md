# Session summary — TUI summaries enriched empty state

## Goal

Continue TUI summaries empty-state polish by making the no-records state more visually prominent and recoverable after new recorded summaries land.

## Bead(s)

- `bd-3e3ba2` — TUI summaries: enrich no-summary empty state
- related: `bd-a5e2fa` — Session-summary viewers across TUI, caco-web, and Android

## Before state

- The TUI no-summary state explained that summaries are written by the `session-recording` profile mixin.
- It included the on-disk summary path.
- It did not visually distinguish the headline strongly, and it did not explicitly remind operators to refresh after new summaries are reintegrated.

## After state

- The no-summary headline is now NORD-accented and bold.
- The existing session-recording explanation and on-disk path remain.
- A new recovery line tells operators to press `r` to refresh after agents reintegrate recorded summaries.

## Diff summary

- Commits: current `bd-3e3ba2` implementation commit
- Files touched:
  - `crates/caco-tui/src/views/summaries.rs`
- Tests:
  - `cargo test -p caco-tui summaries --lib` — passed
  - `cargo test-small` — 256 passed
- Behavioural delta: the TUI summaries empty state is clearer, more visually structured, and includes an explicit refresh action.

## Operator-takeaway

When the TUI summaries view has no records, it now reads like a deliberate product empty state rather than plain explanatory text, and it tells the operator exactly how to refresh after summaries land.
