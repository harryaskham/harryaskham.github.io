# Session summary — caco agent prune reaches legacy + non-Completed terminal checkouts

## Goal

Make `caco agent prune` actually reclaim disk on nodes accumulating
legacy worker checkouts. Disk-pressure investigations had repeatedly
flagged ~3.6 GiB of stuck terminal checkouts on this node alone that
the existing retention sweep silently refused to consider, even with
`--include-discarded`. This session closes the two filtering gaps that
caused that.

## Bead(s)

- `bd-ff753c` — Worker checkout cleanup: dead/discarded agent checkouts
  persist on disk indefinitely, contributing to 201 GiB pressure.

Plus session-start hygiene: closed nine in-progress beads
(bd-ab1c38, bd-f72c32, bd-f4f4cd, bd-b9c9eb, bd-a1ec44, bd-09542b,
bd-ebdf72, bd-14e75e, bd-3fa3c6) that landed under msm-3's reintegrate
footer but were left unclosed against this agent.

## Before state

- `caco agent prune --include-discarded --dry-run` on winmini reports
  "0 completed checkout(s)" despite 4 terminal-state checkouts under
  `~/.cacophony/agents/cacophony/` totaling ~3.6 GiB
  (jksa79oi0bk7h7ah 3.1G completed, 7952i5konp36jydc 179M completed,
  v50zrrpgwtw73ev3 178M completed/stopped, 3gjmwd2sgw1xw0nt 177M
  stopped).
- Root causes:
  1. `plan_completed_checkout_retention_sweep` filtered out every
     agent with `ended_at == None`, which is exactly the legacy
     pre-bd-d438bf cohort whose terminal state predated `ended_at`
     stamping.
  2. CLI `dispatch_agent_prune` only considered `Completed` (or
     `Discarded` with the flag), so terminal `Stopped` and `Failed`
     records — also durable disk consumers — were unreachable to
     retention.

## After state

- `plan_completed_checkout_retention_sweep` falls back to `created_at`
  for the age math when `ended_at` is missing, instead of dropping
  the agent on the floor. Legacy terminal checkouts now age in.
- `caco agent prune --include-discarded` accepts `Completed`,
  `Discarded`, `Stopped`, and `Failed`. Help text updated to reflect
  the broader scope.
- Unit tests: 4/4 retention planner tests pass, including the new
  `completed_retention_plan_falls_back_to_created_at_when_ended_at_missing`
  which asserts both `Completed` and `Stopped` legacy agents (no
  `ended_at`) become eligible against a 24h policy via the
  `created_at` fallback.
- `cargo test-small`: 57 pass, 0 fail.
- `cargo clippy -p caco-daemon -p caco-cli --tests`: clean.

## Diff summary

- Files touched:
  - `crates/caco-daemon/src/lib.rs` — planner: `ended_at.unwrap_or(created_at)`;
    new test `completed_retention_plan_falls_back_to_created_at_when_ended_at_missing`.
  - `crates/caco-cli/src/lib.rs` — `dispatch_agent_prune` state filter
    now matches `Completed | Discarded | Stopped | Failed` (when
    `--include-discarded`); `--include-discarded` arg help updated.
- Tests: +1 unit test, 0 removed, 0 flipped.
- Behavioural delta: legacy and non-Completed terminal worker
  checkouts are now reachable to `caco agent prune`, unblocking
  disk reclamation that the sweep previously silently skipped.

## Operator-takeaway

If a node still shows persistent disk pressure from
`~/.cacophony/agents/<project>/` after this lands, run
`caco agent prune --include-discarded --dry-run` then re-run
without `--dry-run`. Anything still stuck will now be a *new*
filter gap (e.g. an exotic terminal state we missed, or `pruned=true`
records whose checkout dir was never actually deleted) — file a
follow-up bead with the dry-run output rather than assuming the
existing retention path can already see it.
