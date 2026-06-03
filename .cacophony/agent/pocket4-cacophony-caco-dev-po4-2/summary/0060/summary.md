# bd-33b413: complete beads-handler async-blocking offload (recurring wedge fix)

## Problem
RECURRENCE of the fleet-wide beads-authority wedge: helsinki `/api/v1/node`
answers 200 while beads request handlers return transport nonresponse, blocking
bead-filing fleet-wide ~hourly under load, cleared only by a helsinki daemon
restart. bd-962b83 + bd-f18063 offloaded create/spoken-names/get/update/delete/
close/unclaim + claim/unclaim/update_routed + list, but MANY remaining beads
handlers still ran synchronous blocking SQLite/git store ops directly on async
tokio worker threads, so under sustained load they kept starving the runtime
pool and wedging transport. (Note: the cited helsinki build 85be652a1 predates
bd-962b83's e60ccf147, so create/spoken-names were already fixed on main; this
bead completes the remaining handlers.)

## Fix
Route the remaining blocking beads-store ops through the shared
`run_blocking_store_op(op_name, op)` helper (bd-f18063): bounded
`timeout(beads_store_op_timeout(), spawn_blocking(op))`, default 30s, env
`CACO_BEADS_STORE_OP_TIMEOUT_SECS`. Coordinated split with po4-3, who took the
two hot HTTP aggregate reads (`handle_all_beads` /api/v1/beads/all, bd-5f28d7
838a76b25); this bead owns the rest.

### HTTP handlers offloaded (this slice)
- handle_claim_bead (claim/claim_next_ready/claim_queued_dispatch) + the
  no_ready_beads_response helper (made async, list offloaded)
- handle_local_bead_count (count_by_status), handle_bead_stats (lifecycle_stats)
- handle_bead_has (get_bead), handle_list_bead_attachments, handle_read_bead_attachment
- handle_queue_bead_dispatch, handle_clear_bead_dispatch
- handle_beads_status (whole per-project sweep: count_by_status + list_ready +
  lifecycle_summary + checkout-health, hoisted into one spawn_blocking)
- LLM/changelog path: handle_expand_beads (context list + create loop),
  handle_rewrite_bead (context list), handle_refine_beads (target gets + context
  list + 2 updates), handle_changelog (closed list)

### Maintenance/background fns (leading bulk reads offloaded)
- reconcile_stale_in_progress_assignees (in_progress list)
- failover_reclaim_unavailable_beads (in_progress + open lists)
- unclaim_beads_for_failed_agents (in_progress + permanent claimed lists)
Per-bead point mutations in these loops are interleaved with async landing
validation and stay inline (short, bounded, lower risk than the bulk reads).

## Validation (queued, merged base post-838a76b25)
- cargo check -p caco-daemon — pass
- cargo clippy -p caco-daemon -- -D warnings — pass
- cargo test -p caco-daemon --lib bd_962b83 — 4 passed (the bounded-timeout
  helper contract these handlers rely on: env parsing, success, error
  propagation, wedged-store 1s-bound-vs-5s-sleep timeout in <4s)

## Coordination
Bounced from msd-1; clean dedup→split→land-first→converge with po4-3 (their
bd-5f28d7 for handle_all_beads). helsinki must pick up >= this landing to stop
restarting as the stopgap.

## SPEC
SPEC 18.2 plus the cross-cutting async-handler non-blocking contract shared with
bd-e32dba / bd-53239e / bd-962b83 / bd-f18063.

## Diff
See the reintegration receipt for the landed squash SHA.
