# Session summary — bd-5e5ed2: replicate choices across the mesh promptly

## Goal

Fix the operator-reported bug (Harry) that `caco choices` operator-decision
blockers don't propagate across the mesh: a choice presented by an agent on one
node stayed invisible on every other node's operator surfaces "for ages" until
the operator manually logged into the originating node's TUI. This defeats the
whole point of choices, which per the SPEC contract must surface promptly
cluster-wide (TUI / web / Android / macOS / speech).

## Bead(s)

- `bd-5e5ed2` — Choices don't propagate across the mesh promptly. P1 bug,
  caco-daemon/choices/cluster/mesh/multinode/replication.

## Before state

- Choice present/resolve handlers (choices.rs) appended the feed event to the
  LOCAL store and published it to LOCAL UI subscribers, but never called
  `replicator.fan_out()` — unlike every other replicated event (SPEC 12.4). So
  choices were never proactively forwarded to peers and only travelled via
  periodic full-state push (default OFF; 60s when on).
- The live per-event feed ingest handler (lib.rs handle_feed_ingest) had no
  arm for ChoicePresented/ChoiceResolved, so even a delivered choice event was
  appended to the peer's feed but never merged into its pending-choice state.
- Failing tests: none (the bug is a replication-wiring gap, not a test failure).

## After state

- Failing tests: none from this change. choices:: 16/0 green; clippy
  `-p caco-daemon --lib` clean.
- choices.rs: all three choice-event emit sites (present, resolve, and the
  blocking controller present-and-wait) now call
  `state.replicator.fan_out(&feed_event, &state.store)` after append, so choice
  presents AND resolutions fan out to peers immediately (failures retry via the
  undelivered log).
- lib.rs handle_feed_ingest: added a
  `ChoicePresented | ChoiceResolved => merge_choice_feed_event(...)` arm so
  live-ingested choice events surface into operator_inbox on arrival, matching
  the full-state dump path (bd-5b1509).

## Diff summary

- Code commit: pending final squash SHA from the reintegration receipt.
- Files touched: crates/caco-daemon/src/choices.rs (+18),
  crates/caco-daemon/src/lib.rs (+14).
- Tests: +0 (relies on existing merge_choice_feed_event unit tests + the proven
  fan_out / full-state-merge patterns this mirrors).
- Behavioural delta: a choice presented/resolved on node A reaches every peer's
  operator surfaces within ~one mesh round-trip instead of waiting for periodic
  full-state sync; resolution fan-out means urgency clears promptly cluster-wide.

## Embedded artefacts

- none.

## Operator-takeaway

The choices feature had the persistence and UI plumbing but was missing the
actual mesh-replication wiring on BOTH ends: the origin never fanned the event
out to peers (it only persisted + showed it locally), and even if it had, peers
had no per-event ingest arm to merge it into their pending-choice state. Both
gaps are now closed by reusing the established append+fan_out and
full-state-choice-merge patterns. The earlier "enable push and pull sync" change
couldn't fix this because the choice event never entered the proactive per-event
fan-out path in the first place. AFK-timeout auto-fire / escalation modes remain
separate slice-2 work.
