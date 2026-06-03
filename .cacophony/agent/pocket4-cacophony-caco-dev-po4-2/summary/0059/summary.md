# bd-962b83: beads-write handler async-blocking fix (beads-authority arm)

## Problem
The helsinki beads authority wedged fleet-wide: `/api/v1/node` answered 200 while
ALL beads endpoints returned transport nonresponse, blocking bead-filing for every
agent on every node. Root cause = the same async-handler-blocking anti-pattern as
bd-e32dba (collect_telemetry on /api/v1/node) and bd-53239e (AgentManager::stop):
beads handlers ran synchronous blocking SQLite/git ops directly on async tokio
worker threads, starving the runtime pool under contention. daemon.log showed
`/api/v1/beads/spoken-names` timing out while node-probe stayed 200.

## Fix
Offload blocking beads-store ops to a bounded `spawn_blocking` so runtime workers
stay free and a wedged store surfaces a structured error instead of hanging.

Converged on po4-3's shared helper `run_blocking_store_op(op_name, op)` (landed in
bd-f18063, 305fa03b7): `timeout(beads_store_op_timeout(), spawn_blocking(op))`,
default 30s, overridable via `CACO_BEADS_STORE_OP_TIMEOUT_SECS`. po4-3 owned the
claim/unclaim/update _routed helpers + handle_list_beads; this bead owns the rest.

### Handlers offloaded (this slice)
- `handle_create_bead` (create + claim-after-create + attachments cluster)
- `handle_all_bead_spoken_names` (the explicitly-named spoken-names sweep)
- `handle_get_bead`, `handle_delete_bead`, `handle_close_bead`, `handle_unclaim_bead`
- `handle_assigned_beads` (primary list)
- `handle_update_bead` (primary update + attachment sub-block + cross-project
  move delete/insert)

### Regression tests (bd_962b83)
- env override parsing (default / positive / zero / non-numeric fallback)
- success value passthrough
- store-error propagation (NotFound)
- wedged-store bounded timeout: 1s bound vs 5s blocking sleep returns a Db timeout
  error in <4s, proving the await preempts.

## Coordination
Collided with po4-3 (their dup bd-c85e87 closed); split non-overlapping slices and
converged on their shared helper as the single primitive. helsinki controller's
daemon restart was the operator-gated stopgap; this code fix makes it self-recovering.

## Validation
- `cargo check -p caco-daemon` — pass (queued)
- `cargo clippy -p caco-daemon -- -D warnings` — pass (queued)
- `cargo test -p caco-daemon --lib bd_962b83` — 4 passed (queued)

## SPEC
SPEC 18.2 (beads write/push-pending), and the cross-cutting async-handler
non-blocking contract shared with bd-e32dba / bd-53239e.

## Diff
See the reintegration receipt for the landed squash SHA.
