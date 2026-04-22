# Session summary — bd-1d0e14: caco summary registry-of-truth row

## Goal

Fix `caco summary` agent counter under-reporting (Issues 3+4 split out
of bd-7ace15): `agents_failed`/`agents_stopped` always 0;
`agents_started`/`agents_completed` lag at windows <1d.

## Bead(s)

- `bd-1d0e14` — caco summary agents counters under-report

## Before state

- `caco summary --since 30m` reported `agents_failed=0` and
  `agents_stopped=0` even though `caco doctor` saw 12+ failed agents.
- `agents_started`/`agents_completed` lagged for windows <1d.
- Operators could not get a real-time view of agent state from
  `caco summary`; had to cross-reference `caco agent list`.

## Investigation findings

On ms-mac daemon, sqlite query against feed_events:

  agent_completed | last 22h ago
  agent_started   | last 20h ago
  agent_pruned    | last few minutes

Multiple spawn paths in caco-daemon (queue-drain at lib.rs:6883,
persistent-spawn at 27052/27143/27315, mode-spawn at 27488, direct-spawn
at 31461) each call `logger.agent_started()` which writes to
feed_events. At least one of those paths is silently failing to
append, but `agent_pruned` (also lifecycle event) keeps writing. So
the failure is path-specific, not a global feed_events outage.

Filed bd-11ab34 to root-cause which spawn path drops events and add
instrumentation. Not blocking the user-visible improvement.

## After state

Daemon (`crates/caco-daemon/src/lib.rs`):
- `SummaryResponse` now includes `agents_registry { running, completed,
  failed, stopped, other }`.
- Populated from `state.agents.list_all_with_disk_refresh()` —
  registry-of-truth, same source `caco agent list` and `caco doctor`
  use.

CLI (`crates/caco-cli/src/lib.rs`):
- Text formatter renders a new line:
  `Registry: N running, N completed, N failed, N stopped, N other (current)`
- Omits the row when older daemons don't include the field
  (back-compat).
- 2 new unit tests:
  `registry_row_emitted_when_present`,
  `registry_row_omitted_for_older_daemons`.

## Diff summary

- Commit `487d4d27`
- Files: 2 (`crates/caco-daemon/src/lib.rs`, `crates/caco-cli/src/lib.rs`)
- Net: +120 lines
- Tests: 2 new; `cargo test-small` 57 pass; clippy clean.

## Operator-takeaway

`caco summary --since 30m` now shows a Registry row with the
right-now agent counts so under-reporting from the windowed
event-derived counts is no longer invisible. Root-cause for the
under-reporting itself is bd-11ab34.
