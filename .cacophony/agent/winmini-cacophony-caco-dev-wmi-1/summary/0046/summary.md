# Session summary — bd-205791: depends_on_node gating on startup reconcile

## Goal

Apply bd-205791 reachability gating on the daemon's
*startup* reconcile path, not just the periodic one.
Cross-node controllers were crash-looping their init.sh
for several minutes after each daemon restart while
waiting for the periodic reconcile to apply the gate.

## Bead(s)

- `bd-205791` — Cross-node controller persistents: gate
  launch on PeerReachabilityMap[remote_node]
  (P3 feature, refinement of bd-cc441e)

## Before state

- `caco-config::PersistentAgentDecl::depends_on_node`
  field exists (slice 1 already landed).
- `update_depends_on_node_availability` helper +
  `collect_reachable_nodes` exist and are wired into the
  *periodic* reconcile loop (lib.rs:7354).
- The *startup* reconcile (lib.rs:5313) was missing the
  gate — first-launch attempts on daemon restart could
  fail their init.sh against a peer that was down,
  burning restart budget for ~1 cycle (~1-5min) before
  the periodic loop's gate kicked in.

## After state

- Startup reconcile now: drops sentinel, gathers
  `reachable_nodes`, re-acquires sentinel, calls
  `update_depends_on_node_availability`, and adds
  `depends_blocked` to the action-filter chain alongside
  `resume_succeeded` and `profile_unavailable`.
- Behavior matches the periodic loop: blocked agents stay
  Pending with `last_error = "depends_on_node X
  unreachable"`, no restart budget consumed.

## Diff summary

- Files touched (+22 / −2):
  - `crates/caco-daemon/src/lib.rs`: startup reconcile
    block adds reachability snapshot + depends_blocked
    filter.

## Verification

- `cargo build -p caco-daemon`: clean.
- `cargo test -p caco-daemon --lib
  update_depends_on_node_availability`: 2 pass (the
  helper's existing tests cover the blocking + clearing
  matrix; this change just routes one more caller
  through it).

## Operator-takeaway

Cross-node controller persistents (e.g. monitor-on-node-A
that polls daemon-on-node-B) no longer crash-loop on
daemon restart when the monitored peer is down. They stay
politely Pending with a clear `last_error` until the peer
comes back, then start cleanly on the next reconcile.
