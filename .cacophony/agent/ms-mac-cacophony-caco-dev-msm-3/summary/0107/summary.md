# Session summary — direct agent status disk refresh

## Goal

Address the recurring `bd-2efa9c` stale-status mismatch where fleet/list surfaces and direct `caco agent status --id` could disagree about whether active workers were stale, even after the impossible `>65535s` age formatting had been fixed.

## Bead(s)

- `bd-2efa9c` — Active workers remain marked stale with impossible no-tool-activity age

## Before state

- Failing tests: none in-tree; router health passes showed live operational mismatches.
- Relevant metrics: repeated health evidence showed canonical stale lists, fleet summaries, and direct agent status disagreeing within the same bounded pass; the latest examples were direct stale reports for `cqqcqwtn68gktqrr` and `o99l01hnn52l2547` while the canonical stale list was empty.
- Context: aggregate inventory used `list_all_with_disk_refresh()`, but direct per-agent status used the in-memory map directly, so newer `agent.json` liveness persisted by short-lived control paths could be missed by direct status.

## After state

- Failing tests: none in targeted validation.
- Relevant metrics: added two daemon regression tests covering per-agent disk refresh and existing-row aggregate refresh; `cargo test -p caco-daemon disk_refresh -- --nocapture` passes all five matched tests; `cargo check -p caco-daemon --tests` passes.
- Context: direct status now calls `get_with_disk_refresh()`, and aggregate disk refresh updates existing rows when disk carries fresher lifecycle/liveness truth instead of only inserting missing agents.

## Diff summary

- Commits: `6ed60057b`
- Files touched: `crates/caco-daemon/src/agent/lifecycle.rs`, `crates/caco-daemon/src/agent/tests.rs`, `crates/caco-daemon/src/lib.rs`
- Tests: `cargo fmt --all -- --check`, `cargo test -p caco-daemon disk_refresh -- --nocapture`, and `cargo check -p caco-daemon --tests`.
- Behavioural delta: direct per-agent status and aggregate list/status surfaces now converge on fresher persisted liveness instead of serving stale in-memory `Stalled` snapshots after a liveness ping has reached disk.

## Operator-takeaway

This closes the remaining observed `bd-2efa9c` mismatch class by making direct status use the same durable disk truth refresh pattern as aggregate inventory surfaces, while preserving terminal-state and fresher-running safeguards.
