# Session Summary — bd-36d913 (beads-claim transport classification)

## Bead
**bd-36d913** (P2 bug, labels: api, beads, daemon)
"caco bd claim endpoint returns handler-transport nonresponse on ms-dev-2 (node v1.2.1065)"

Picked up via an explicit cross-node handoff: cs-0 (caco-dev-cs-0-2) implemented and
queue-validated the fix but could not land it from cs-0 (corrupt partial clones, bd-0bf13e).
The scratch note `bd-36d913-patch` did not replicate cluster-wide (confirmed unreadable from
po4-3 and cs-1-0), so the full patch was also embedded in the bead description. po4-3 (healthy
node) reconstructed + cross-checked against cs-0's embedded patch and landed it.

## Root cause
The blocking bd request path (`bd_send_request`, used by `caco bd claim`) routed a transport
nonresponse through the generic `daemon_endpoint_nonresponse` envelope (retryable: false,
"file an endpoint-specific daemon bug") instead of the retryable per-endpoint classification
the agent-create/resume/lifecycle/msg-send paths already use. An idle auto-claim worker that
hit a transient claim-handler nonresponse was told to file a daemon bug rather than retry.

## Fix (crates/caco-cli/src/lib.rs)
- `is_beads_claim_url_path` — matches both `/api/v1/projects/{project}/beads/claim` (no-ID,
  bead in JSON body) and `/api/v1/projects/{project}/beads/{id}/claim`.
- `beads_claim_transport_failure_message` / `beads_claim_transport_failure_envelope` —
  retryable-with-inspection classification mirroring agent-create: `code:
  beads_claim_transport_error`, `retryable: true`, `retry_after_secs:
  ENDPOINT_TRANSPORT_RETRY_AFTER_SECS`, shared `mutation_transport_class_labels`,
  `may_be_in_progress: true`, duplicate-avoidance pointing at `caco bd show --bead-id <id>` /
  in-progress inspection so an already-accepted claim is not duplicated or raced.
- `bd_transport_error_envelope` / `bd_transport_cli_error` — blocking, claim-path-aware
  wrappers that check the sidecar restart envelope first, then route claim-URL transport
  failures to the retryable classification, else delegate to the existing generic
  `transport_error_envelope` / `transport_cli_error` unchanged.
- `bd_send_request` transport-error branch now calls the bd-aware wrappers.
- `ENDPOINT_TRANSPORT_CLASSIFICATIONS` gains a `beads_claim_transport_error` row.

## Tests (caco-cli lib)
- `is_beads_claim_url_path_matches_claim_post_bd_36d913` — both claim URL shapes match;
  non-claim beads/agent/node paths do not.
- `beads_claim_transport_error_is_structured_bd_36d913` — daemon-alive envelope is retryable,
  carries the mutation labels, names the claim surface, points at in-progress inspection.
- `beads_claim_transport_error_probe_unavailable_is_still_retryable_bd_36d913` — probe-
  unavailable stays retryable, only `unexpected` flips true.
- `classify_endpoint_transport_finds_all_known_surfaces_bd_9a70b9` updated to include
  `beads_claim_transport_error` + assert its `duplicate_avoidance.is_some()`.

## Validation (queued on shared host per merge-queue policy)
- `cargo check -p caco-cli` → passed.
- `cargo test -p caco-cli --lib bd_36d913` (RUST_MIN_STACK=33554432) → 3 passed.
- `cargo test -p caco-cli --lib classify_endpoint_transport_finds_all_known_surfaces` → passed.
- `cargo clippy -p caco-cli --lib -- -D warnings` → passed (no new warnings).

## Pre-existing broken-on-main found (NOT mine, filed separately)
`bd_create_transport_failure_after_persistence_reports_indeterminate_create_bd_ae014b` fails
on clean origin/main (verified by stashing all WIP) — the create POST is retried by
`send_blocking_request_with_daemon_read_retries` before the indeterminate-create recovery
list-probe runs, breaking the test's single-POST+GET mock and risking duplicate non-idempotent
creates. Filed as **bd-856ffd** (broken-on-main, caco-cli) with root cause + fix directions;
left open for a create-idempotency owner. It does not block bd-36d913 (separate code path;
the non-test cacophony-fast-tests clippy gate stays green).

## SPEC
Aligns the blocking `caco bd claim` transport-error path with the per-endpoint
retryable-with-inspection classification contract used by agent/msg-send surfaces (SPEC
beads/CLI control-surface; matches AGENTS.md transport-classification guidance). No board/data
behavior change; error-classification only.

## Diff
crates/caco-cli/src/lib.rs (classifier helpers + table row + blocking wrappers + 3 tests +
classify-test update). Landed squash SHA: see reintegration receipt.
