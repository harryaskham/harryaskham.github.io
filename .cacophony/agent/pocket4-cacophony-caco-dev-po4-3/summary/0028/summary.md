# Session Summary — bd-5f28d7 (offload handle_all_beads aggregate read)

## Bead
**bd-5f28d7** (P1 bug; child of **bd-33b413**; labels caco-daemon/beads/reliability/async/broken-on-main)
"Offload handle_all_beads (/api/v1/beads/all) aggregate read off async worker + hoist
build_assignee_agent_status_lookup out of per-project loop"

Part of the recurring beads-authority async-blocking wedge fix, under an agreed non-overlapping
split with po4-2 (who owns parent bd-33b413). Lands the land-first slice po4-2 then converges on.

## Context: recurring beads wedge
The helsinki beads primary kept wedging (~hourly under load) with transport nonresponse on beads
endpoints while /api/v1/node stayed up. Root cause is the async-handler-blocking class (sibling to
bd-53239e agent-stop, bd-e32dba /api/v1/node, bd-f18063 + bd-962b83 beads handlers): synchronous
blocking BeadsStore SQLite/git calls run directly on the async worker, so under concurrent load every
Tokio worker blocks and no beads handler future can be polled.

After bd-f18063 (claim/unclaim/update routed + handle_list_beads) and bd-962b83
(create/spoken-names/get/update/delete/close/unclaim + tests) landed, I enumerated the REMAINING
un-offloaded blocking store calls still on async workers and handed them to po4-2. We split the
remainder; this bead is my slice: the hot `/api/v1/beads/all` aggregate read.

## Change (`crates/caco-daemon/src/beads.rs`, `handle_all_beads`)
1. **Hoisted `build_assignee_agent_status_lookup(&state).await` out of the per-project loop.** It is
   project-independent (depends only on `&state`) but was recomputed once per project inside the loop.
   Now computed ONCE before the loop. Correctness-preserving perf win AND it removes the only `.await`
   inside the loop body, which is what enables the blocking offload.
2. **Wrapped the entire per-project blocking sweep in one `run_blocking_store_op` call** (the shared
   primitive from bd-f18063). The sweep — `cached_beads_store` + `get_bead` (blocks-of resolution) +
   `list_ready()` / `list_beads()` per project — now runs on the blocking pool instead of the async
   worker. Owned inputs (state Arc clone, projects, status_filter, bead_types, labels,
   remote_live_bead_ids, the needed `query.*` fields, and the hoisted lookup) are cloned into the
   `Send + 'static` closure; the borrowed `AggregateQueryView` is rebuilt from owned locals inside the
   closure. The closure returns `(Vec<GlobalBeadRow>, Vec<GlobalBeadsProjectFailure>)`; a sweep error
   maps to a `503 + Retry-After` `beads_all_sweep_failed` envelope.

Response shape (`AllBeadsResponse`: beads/count/failed_projects), per-project filtering, multi-value
type/label + assignee post-filtering, sorting, limit, and count_only semantics are all unchanged.

## Explicitly out of scope (po4-2 owns under bd-33b413)
handle_assigned_beads primary list_beads (done in bd-962b83) + its sync no_ready_beads_response helper;
handle_all_bead_spoken_names (done, e60ccf147); claim/count/has/stats/read_attachment/queue+clear
dispatch; the LLM-path handlers; handle_beads_status; and the background/maintenance fns
(reconcile_stale_in_progress_assignees, replay_beads_holding_bay, close_beads_for_completed_agent_notify,
failover_reclaim_unavailable_beads).

## Validation (queued on shared host)
- `cargo check -p caco-daemon --lib` → passed.
- `cargo clippy -p caco-daemon --lib -- -D warnings` → passed.
- `cargo test -p caco-daemon --lib all_beads` → passed.

## SPEC
Preserves the loopback/cluster beads API responsiveness contract: a slow/contended store on the hot
aggregate-read path must not become daemon API transport nonresponse; the bounded handler surfaces a
retryable 503 instead.

## Diff
Landed squash SHA: see reintegration receipt.
