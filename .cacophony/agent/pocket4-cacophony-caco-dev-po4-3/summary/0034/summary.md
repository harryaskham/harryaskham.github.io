# bd-2c0961: beads list/ready/create warmup-gate hardening

## Bead
bd-2c0961 (P2 diagnostic→fix) — beads list/ready endpoint warmup-sensitive:
cluster-wide board-read block during the 1.2.1076 multi-restart window. Filed by
helsinki-cacophony-cluster-ctrl after the live incident; root cause cross-checked
with this agent (po4-3) during the outage.

## Incident recap (root cause confirmed)
Post-1.2.1076 restart on helsinki (beads authority), `GET /…/beads?ready=true`
and `?status=in_progress` failed for ~9 min with "daemon reachable via
/api/v1/node but endpoint failed before any semantic response," while `bd status`
kept serving. Cluster-wide: board list/ready reads **and bead CREATE** blocked
(CREATE proxies through the same path). Confirmed **not** a code regression
(`beads.rs` list/ready handler unchanged) and **not** a route-table break
(`status` served throughout) — a post-restart warmup/db-state transient on the
index-backed list/ready query path during multi-restart convergence, where the
routing-metadata `status` path recovers earlier. It self-cleared once the final
restart converged.

## Fix
The structured warmup-backpressure envelope already existed
(`crate::startup_backpressure_response`) and was already applied to
`/api/v1/beads/all` and `/api/v1/beads/spoken-names`, but **not** to the main
list/ready handler or create. This change adds the same guard to:

- **`handle_list_beads`** (`crates/caco-daemon/src/beads.rs`) — the
  `GET /api/v1/projects/{project}/beads` list/ready path that failed.
- **`handle_create_bead`** — CREATE shares the warmup-sensitive path and has the
  worse blast radius the bead calls out.

Both now return, during post-restart warmup, a retryable
`503 daemon_startup_warming` envelope with `Retry-After: 5` and
`x-cacophony-startup-backpressure: true` — instead of a raw pre-semantic
transport failure that agents are told to file as an endpoint bug. The guard sits
right after `extract_request_id`, before the local/proxy branch, so a warming
authoritative node returns the structured signal to both local and proxied
callers.

### Deliberately NOT gated: `handle_beads_status`
`status` uses the routing-metadata path that recovers earlier and is the
contrast the incident relied on (it stayed up throughout). Leaving it ungated
preserves the operator's ability to see authoritative routing/maintenance state
during warmup. Verified no `startup_backpressure_response` call was added to the
status handler.

## Tests
2 new daemon tests (mirroring the existing
`ui_snapshot_returns_retryable_backpressure_during_startup_warmup` /
`agent_create_…` pattern), green with the existing warmup + beads suites via the
daemon queue (`RUST_MIN_STACK=33554432 cargo test -p caco-daemon --lib … -- --test-threads=1`):
- `beads_list_ready_returns_retryable_backpressure_during_startup_warmup_bd_2c0961`
  — `GET …/beads?ready=true` → 503 `daemon_startup_warming` + `Retry-After: 5` +
  backpressure header.
- `beads_create_returns_retryable_backpressure_during_startup_warmup_bd_2c0961`
  — `POST …/beads` → 503 `daemon_startup_warming` + backpressure header.
- Existing `ui_snapshot_…` + `agent_create_…` warmup tests + the broad `beads::`
  suite still pass (no regression).

## Why it matters / blast radius
The list/ready path being unavailable blocks cluster-wide board reads + bead
CREATE during authority-node warmup — a worse blast radius than a status-only
degradation. Returning a retryable maintenance signal lets CLI/agents back off
and retry (consistent with the existing proxy "retry shortly" behavior) rather
than mis-filing an endpoint regression and triggering release holds.

## SPEC areas
SPEC 6.x daemon beads endpoint + startup/maintenance backpressure contract
(`daemon_startup_warming`, `Retry-After`, structured retryable envelopes). No
SPEC contract change — extends an existing graceful-degradation pattern to two
more endpoints.

## Diff summary
Warmup guard added to `handle_list_beads` + `handle_create_bead`; 2 new tests.
Final landed squash SHA per the reintegration receipt.
