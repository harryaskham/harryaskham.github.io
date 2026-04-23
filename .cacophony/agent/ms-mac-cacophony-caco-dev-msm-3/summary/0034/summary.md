# Session summary — bd-11ab34: agent recreate path persists AgentStarted

## Goal

Investigate why agent_started/completed events appear stale in
feed_events on this node despite recent agent activity, and ship the
defensible part of the fix.

## Bead(s)

- `bd-11ab34` — agent_started/completed events drop from feed_events on some spawn paths

## Before state

Codegrep showed 7 spawn paths emitting `feed::EventType::AgentStarted`
in `crates/caco-daemon/src/lib.rs` (6883, 23374, 27106, 27197, 27353,
27518, 31515). Only the entries at 6883 / 27353 / 27518 routed
through `logger.agent_started()` (which writes to feed_events).
The other four called `state.ui_broadcast.publish_feed_event(...)`
directly, reaching only live SSE/UI subscribers.

Reproducer per bead: `agent_pruned` events kept flowing in feed_events
while `agent_started`/`agent_completed` went 22h+ stale.

## Investigation findings

Two distinct causes blend together in the symptom:

1. **Real bug — recreate path (23374)**: Persistent agents auto-recreated
   after crash emit AgentStarted via `ui_broadcast.publish_feed_event`
   only. The feed_events store never sees these. Fixed in this commit.

2. **By design — queued marker (27106) and cross-node spawn (27197)**:
   The queued-marker case is a UI-only placeholder; the real
   `AgentQueued` event fires right after through the canonical
   `beads::emit_feed_event` path. The cross-node case relies on the
   remote node's daemon to emit the canonical event and on feed
   replication to carry it back; emitting locally too would double-
   count.

3. **By design — daemon restart (separate root cause)**: Persistent
   agents spanning a daemon restart get re-attached to their existing
   tmux sessions without re-firing AgentStarted. This is legitimate
   behaviour. The visibility gap in `caco summary` is already
   addressed by the registry-of-truth row added in bd-1d0e14.

## After state

- `crates/caco-daemon/src/lib.rs:23374` recreate path:
  - Replaced `state.ui_broadcast.publish_feed_event(&feed_event)` with
    `beads::emit_feed_event(&state, feed_event).await` (canonical
    store + UI + cluster-fanout helper).
  - Added `state.logger.agent_started(&project, &new_agent_id).await`
    for the dedup'd daemon.log line.
- `cargo test-small`: 57 pass.
- `cargo clippy -p caco-daemon --tests`: clean.

## Diff summary

- 1 commit, 1 file (`crates/caco-daemon/src/lib.rs`)
- Net: +15 / -1 lines (replace one publish call, add one logger call,
  document the bd-11ab34 finding inline)
- Tests: covered by existing `caco summary` integration tests; no new
  tests added (the fix routes through an already-tested helper).

## Operator-takeaway

After-rollout, persistent agents auto-recreated by the daemon (post-
crash recovery, etc.) will show up in `caco summary` agent_started
counts. The other asymmetric paths are by-design and already accounted
for. The under-reporting from daemon restarts is bd-1d0e14's registry
row.
