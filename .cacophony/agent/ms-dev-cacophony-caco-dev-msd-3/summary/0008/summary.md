# Session summary — bd-7f54f7: fix misleading 'caco tendril' hint

## Goal

Remove the misleading `try: caco tendril <action>` hint from the
TUI agent-detail view's tendril-actions empty state. No such CLI
subcommand exists — tendril is a separate project.

## Bead(s)

- `bd-7f54f7` — try: caco tendril <action> from the agent profile
  <<< text in TUI but caco tendril does not exist yet

## Before state

- Agent-detail view showed "try: caco tendril <action> from the
  agent profile" when no tendril actions were recorded.
- Running `caco tendril ...` would fail with an unknown-command
  error, confusing new operators.

## After state

- Hint replaced with "tendril is driven by the agent profile
  (separate `tendril` CLI — not `caco tendril`)" — accurate
  guidance that avoids the dead-command trap.

## Diff summary

- File: `crates/caco-tui/src/views/agent_detail.rs` (+6/-1).
- One-line text replacement + 5-line comment explaining the
  change and referencing bd-7f54f7.
- Build clean: `cargo build -p caco-tui`.

## Embedded artefacts

(none)

## Operator-takeaway

Small UX fix. TUI hints should reference commands that exist.
