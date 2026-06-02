# Session summary — Fix choices not propagating promptly across the mesh (bd-e415c0)

## Goal

Operator-reported bug: when an agent presents a `caco choices` decision on
one node, it stays invisible on other nodes' operator surfaces "for ages" —
the operator has to log into the TUI on the originating node to see and
resolve it. This defeats the urgent-blocker purpose of choices, which per the
SPEC choices contract must surface promptly across ALL operator surfaces
cluster-wide. The goal was to find why proactive replication wasn't making
choices visible on peers and fix it so a choice presented on node A appears on
node B's operator surfaces within a short bounded interval without a manual
TUI login on node A.

## Bead(s)

- `bd-e415c0` — Choices don't propagate across the mesh promptly (need TUI login on origin node to see them)

## Before state

- Failing tests: none introduced by this; the bug was a runtime replication gap.
- Root cause: `Replicator::ingest` (the receive side for proactively
  fanned-out feed events, the bd-5e5ed2 present-time `fan_out`) only called
  `s.append_event(event)` for accepted events. It did NOT call
  `merge_choice_feed_event` for `ChoicePresented` / `ChoiceResolved` events,
  unlike the full-state dump merge path (replication.rs ~4514) and the
  `materialize_remote_histories` pull path (~5651/5669), both of which already
  do this under bd-5b1509. So a fanned-out choice was appended to the peer's
  feed log but never materialized into the peer's durable operator_inbox row,
  which is what `caco choices current`, the TUI/web/Android/macOS choices
  surfaces, and `/api/v1/choices/*` query. The choice therefore only became
  visible once the much slower periodic full-state sync ran
  `merge_choice_feed_event` — exactly the "invisible for ages" symptom.
- The recent "enable both push and pull sync" change did not fix it because the
  push families were already enabled by default; the gap was in the ingest
  receive path's materialization, not in which families were configured.

## After state

- Failing tests: none.
- `Replicator::ingest` now calls `crate::operator_inbox::merge_choice_feed_event`
  immediately for each newly-accepted `ChoicePresented` / `ChoiceResolved`
  event, mirroring the full-state and pull paths. The HTTP `handle_feed_ingest`
  handler already republishes accepted events to `ui_broadcast`, so connected
  peer TUIs also get a live refresh; the missing piece was the durable
  operator-surface materialization, which is now closed.
- Targeted validation (all green, via the daemon test queue):
  - `replication::tests::ingest*` — 6 passed (incl. the new regression test)
  - `operator_inbox::tests` — 24 passed
  - `choices::tests` — 16 passed
  - `cargo clippy -p caco-daemon --lib` — clean

## Diff summary

- Code/content commit: pending final squash SHA from the reintegration receipt.
- Files touched: `crates/caco-daemon/src/replication.rs`
- Tests: +1 (`ingest_materializes_choice_into_operator_surface_bd_e415c0`)
- Behavioural delta: proactively fanned-out choice events now materialize into
  the peer operator_inbox during ingest, so remote operator surfaces see and can
  resolve urgent choices promptly instead of waiting for the periodic full-state
  sync. Aligns the ingest path with SPEC line 6096 (the bd-5b1509 contract) that
  the full-state and pull paths already honored.

## Operator-takeaway

The choices-replication contract (SPEC 6096 / bd-5b1509) was honored by the
full-state dump merge and the pull path, but the proactive single-event
fan-out *receive* path (`Replicator::ingest`) was the one place that appended
the raw choice feed event without materializing it into the durable operator
surface. That single missing `merge_choice_feed_event` call in ingest was why
the bd-5e5ed2 present-time fan_out didn't actually make choices visible on
peers — the event arrived, but nothing turned it into a queryable choice row
until slow periodic sync. Future replication work that adds a new
operator-surface-bearing event family should remember there are three receive
paths to keep in sync: full-state dump merge, full-state pull
(`materialize_remote_histories`), and single-event `ingest`.
