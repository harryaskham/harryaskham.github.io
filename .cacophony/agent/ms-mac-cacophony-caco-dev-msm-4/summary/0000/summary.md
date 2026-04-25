# Session summary — bd-3f31f8 lifecycle-paused agents

## Goal

Take a safe first slice of the Termux/phone sleep phantom-unreachable issue by making agent summaries distinguish nodes with an active lifecycle outage from genuinely stranded agents on actionable-unreachable nodes.

## Bead(s)

- `bd-3f31f8` — sgu24/Termux silent stretches create phantom Unreachable and stranded-agent noise

## Before state

- `agents/summary` treated active agents on unreachable nodes as stranded when the peer health status was actionable-unreachable.
- Planned lifecycle outage state from `caco daemon lifecycle sleep/shutdown/update` was visible on node APIs but not used by agent-summary stranded accounting.
- CLI summary text had only Potentially Stuck, Stranded, and Stranded Agents sections; there was no lifecycle-paused bucket.

## After state

- `agents/summary` now builds an active planned-outage map and excludes those nodes from unreachable/stranded accounting while keeping their agents visible.
- Potentially stuck entries now include a `node_lifecycle` object when the hosting node announced sleep/shutdown/update.
- `caco agent summary` renders those rows under `Lifecycle-Paused`, using `idle=?` to show the activity counter is last-known rather than proof of a wedged worker.
- Added daemon and CLI regression coverage for lifecycle-paused handling.

## Diff summary

- Commit: `81bf79ef3` after replay onto the remote agent branch.
- Files touched: `crates/caco-daemon/src/lib.rs`, `crates/caco-cli/src/lib.rs`.
- Tests: `cargo test -p caco-daemon agents_summary_marks_lifecycle_paused_nodes_without_stranding_agents --lib`; `cargo test -p caco-cli agent_summary_text_separates_actionable_and_advisory_node_health --lib`; `cargo check -p caco-daemon --tests`; `cargo fmt --all -- --check`; `git diff --check`.
- Behavioural delta: sleeping/updating nodes no longer create misleading stranded-agent counts when they have an active lifecycle announcement.

## Operator-takeaway

This does not fully solve phone sleep detection, but it wires the existing lifecycle signal into the summary path so planned/device sleep stops looking like an actionable stranded-agent incident.
