# Session Summary — bd-f18063 (beads async-blocking: shared helper + claim/unclaim/update/list slice)

## Bead
**bd-f18063** (P1 bug; child of **bd-962b83**; labels caco-daemon/beads/reliability/async/broken-on-main)
"Beads async-blocking fix: shared run_blocking_store_op helper (spawn_blocking + bounded
timeout) + claim/unclaim/update routed + handle_list_beads"

Lands the **shared primitive first** for the beads-handler async-blocking wedge fix, under an
agreed non-overlapping split with po4-2 (who owns parent bd-962b83).

## Context: the fleet-wide beads wedge
Helsinki (sole beads primary, no failover) suffered a fleet-wide outage where `/api/v1/node`
and `caco msg` stayed healthy but ALL beads endpoints (`bd list/status/search/claim`,
`/api/v1/beads`, project beads CRUD) returned "transport nonresponse before any semantic
response" on both the cluster proxy and helsinki's loopback. The controller recovered it with
a daemon restart and filed bd-962b83 as the canonical root-cause bead.

Root cause (same class as bd-53239e agent-stop arm and bd-e32dba /api/v1/node): the beads
handlers call synchronous blocking `BeadsStore` SQLite/git methods directly on the async
worker thread (no `.await` yield point). Under concurrent beads load every Tokio worker
blocks, so the runtime cannot poll any beads handler future → fleet-wide nonresponse.

## This slice (`crates/caco-daemon/src/beads.rs`)
1. **`run_blocking_store_op(op_name, closure)` shared helper**: wraps
   `tokio::task::spawn_blocking` AND bounds it with a `CACO_BEADS_STORE_OP_TIMEOUT_SECS`
   timeout (default 30s), returning `BeadsError::Db` on join-failure or timeout. The bound is
   the key improvement over a bare spawn_blocking: offload alone keeps the async worker free,
   but only a timeout lets the handler actually preempt a genuinely-slow/contended store and
   return a clean retryable error instead of parking a blocking-pool thread indefinitely.
   (po4-2 is adopting this as the shared primitive across the whole fix.)
2. **Applied** to the local-store (`handles_beads_locally`) branches of the canonical routed
   mutators `claim_bead_routed`, `unclaim_bead_routed`, `update_bead_routed`, and the hot
   `handle_list_beads` read path (the `bd list` arm that wedged). `BeadsStore` is
   `Arc<...>: Send + Sync`, so the offload clones the Arc into the closure.

## Split discipline (agreed with po4-2)
- **This child (po4-3)**: helper + claim/unclaim/update routed + handle_list_beads. Lands FIRST.
- **po4-2 (under bd-962b83)**: create + spoken-names handlers (already green on their branch,
  to be refactored onto this helper), then close/get/delete/attachments/status handlers + a
  deliberately-slow-store regression test.
- Zero overlapping handlers; one shared helper.

I originally filed bd-c85e87 for the whole fix before seeing the controller's all-clear naming
bd-962b83 (claimed by po4-2); I closed bd-c85e87 as a duplicate, offered my WIP, and po4-2
chose this non-overlapping split adopting my helper.

## Validation (queued on shared host)
- `cargo check -p caco-daemon --lib` → passed.
- `cargo clippy -p caco-daemon --lib -- -D warnings` → passed.

## SPEC
Preserves the loopback/cluster beads API responsiveness contract — a slow/contended store
must not turn into daemon API transport nonresponse; bounded handlers surface retryable errors.

## Diff
Landed squash SHA: see reintegration receipt.
