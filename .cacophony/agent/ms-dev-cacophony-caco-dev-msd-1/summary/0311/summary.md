# Session summary — bd-8999ca: /api/v1 reintegration dead-letters HTTP endpoint

## Goal
Add the read-only daemon `/api/v1` HTTP endpoint exposing reintegration dead-letters, so the macOS /
web / Android Operations surfaces can fetch + display them during a failed-reintegration incident.
This is the daemon-half prereq for bd-b0e193 (macOS Operations UI); the bead is explicitly scoped to
"the daemon endpoint only" (the UI half is the separate caco-macos-lane bd-b0e193).

## Bead(s)
- bd-8999ca (P2 feature, daemon). Single-bead scope (daemon endpoint only) — clean close, no remaining
  scope. Prereq for bd-b0e193 (macOS UI, caco-macos lane).

## Before state
Reintegration dead-letters were only reachable via the CLI (`caco agent dead-letters list` / `caco
ops`) and the persisted `reintegration_dead_letters` daemon.db rows. There was NO `/api/v1` HTTP
endpoint, so the macOS / web / Android Operations surfaces could not fetch them. The merge-queue
list (`/api/v1/merge-queue`) was already exposed, but dead-letters were not.

## After state
- New read-only endpoint `GET /api/v1/reintegration-dead-letters`, registered on BOTH the
  local_router (loopback bearer-token) and cluster_router (mTLS), mirroring `/api/v1/merge-queue`.
- `handle_reintegration_dead_letters_list`: locks the daemon store, calls the existing
  `reintegration_dead_letter::list_records(db, project, agent_id, limit)`, returns the merge-queue
  list response shape `{ data: [records], request_id }`; on read error returns a 500 ErrorEnvelope
  ("reintegration_dead_letters_read_error"). Query params: project, agent_id, limit (default 50,
  clamp 1..=500).
- Records serialize the operator-incident fields the bead asked for (already on
  ReintegrationDeadLetterRecord, which is Serialize): id (rdl-...), agent_id, project, branch/head,
  target_branch, mode, failure_class, conflict_paths, attempt_id, status (open/retry_requested/
  retried/retry_failed/discarded), resolution, resolved_at/by, timestamps, message.
- Read-only; no mutating side effects, no land/publish-path change. Reuses the existing tested
  list_records query (no new SQL).
- 1 endpoint test (seed a dead-letter into test_state's store → GET → assert 200 + the record +
  fields). caco-daemon compiles clean; test passes.

## Diff summary
- crates/caco-daemon/src/lib.rs: ReintegrationDeadLettersQueryParams + handle_reintegration_dead_letters_list
  (after handle_merge_queue_list); route registered in local_router + cluster_router; 1 oneshot test.
(Final landed squash SHA: see the reintegration receipt.)

## Operator takeaway
Reintegration dead-letters are now fetchable over HTTP at `GET /api/v1/reintegration-dead-letters`
(loopback + mTLS), mirroring the merge-queue list endpoint — unblocking the macOS / web / Android
Operations panes (bd-b0e193) to surface stranded reintegrations during an incident. Read-only,
reuses the existing dead-letter list query, no land-path touch.
