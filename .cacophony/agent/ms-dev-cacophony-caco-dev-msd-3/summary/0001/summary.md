# Session summary — TUI agent detail one-line collapsed header

## Goal

Make the TUI Agent Detail metadata header genuinely collapsible down to a single compact row so operators can recover screen space while still seeing the selected agent identity, state, and action buttons.

## Bead(s)

- `bd-dbea86` — Make TUI agent detail header collapsible

## Before state

- Failing tests: none known at claim time.
- Relevant metrics: the Agent Detail header already had per-column collapse controls, but the fully-collapsed state still reserved a three-row section and rendered a two-line summary with node, bead, diff, and expansion hints.
- Context: the operator request specifically asked for a single line with just buttons, agent id, state, and buttons rather than a dense metadata block.

## After state

- Failing tests: none in focused validation.
- Relevant metrics: queued test `tj-10b052d6` passed `CARGO_BUILD_JOBS=2 cargo test -p caco-tui --lib all_columns_collapsed -- --test-threads=1`.
- Context: Alt+0/full collapse now makes the agent and persistent-agent detail header height one row; that row renders ID, compact state, an expand hint, and shares the row with the existing action/recovery button renderer.

## Diff summary

- Code/content commits: `f0fae2fe73`.
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA.
- Files touched: `crates/caco-tui/src/views/agent_detail.rs`, `SPEC.md`, `.cacophony/agent/ms-dev-cacophony-caco-dev-msd-3/summary/pending/summary.md`.
- Tests: updated focused render tests for regular and persistent agent detail collapsed summaries; added a one-line height assertion. Initial validation command `tj-d50ef3f4` failed because Cargo only accepts one filter before `--`; the corrected shared-filter run `tj-10b052d6` passed.
- Behavioural delta: fully collapsed Agent Detail headers are now a one-row command strip instead of a compact-but-still-multi-row metadata panel.

## Operator-takeaway

Alt+0 on Agent Detail now does what the operator asked: reclaim vertical space while leaving the agent id/state and buttons visible, with expanded metadata still available by toggling back open.
