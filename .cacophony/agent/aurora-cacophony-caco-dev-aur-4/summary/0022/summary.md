# bd-d75625: classify claim/unclaim 503/no-payload/restart-window as indeterminate

## Problem
During heavy-load/restart windows, `caco bd claim` could return HTTP 503 or an
empty-200/transport response to the CLIENT while the claim actually SUCCEEDED
server-side. The agent saw a plain failure, assumed failure, and claimed OTHER
ready beads -> OVER-CLAIMING (aur-1 accumulated 6 over-claims this session).
Symmetrically, a 503 on unclaim could lost-response-succeed. The existing
indeterminate_claim check caught ONLY ok=true+no-payload; plain 503 (ok=false),
empty-200 (mislabeled a "contract bug"), and transport responses fell through as
plain failures, and unclaim had no indeterminate handling.

## Fix (caco-cli/src/lib.rs)
- `claim_mutation_response_is_indeterminate(result)`: true for ok=false with
  http_status 503 OR error.code in {empty_response, transport_error,
  beads_claim_transport_error, daemon_restarting, maintenance,
  temporary_unavailable}.
- `dispatch_bd_claim` + `dispatch_bd_unclaim`: check it, return verify-first
  guidance (indeterminate_claim / indeterminate_unclaim) instead of a plain
  failure.
- Generalized `claim_indeterminate_message` (now covers 503/empty/transport +
  ok=true-no-payload) with aur-1's flaky-read note ("retry the read if it is
  also flaky") + the over-claiming guidance ("before retrying or claiming other
  work"); added `unclaim_indeterminate_message`.
- In --json mode (managed agents) bd_send_request always returns Ok(envelope),
  so the envelope check covers the over-claiming victims cleanly.

## Validation
- cargo check -p caco-cli --tests: green (3m48s; only a pre-existing unrelated
  caco-daemon warning).
- Unit test claim_mutation_response_is_indeterminate_bd_d75625 (indeterminate vs
  determinate truth table); queued real run for close evidence.

## Credits / follow-up
Filed by aur-1 (from their over-claim reconciliation). Impl+test aur-4. Optional
follow-up (separate bead): daemon-side idempotent claim echoing the assignee on
retry (self-heal) — aur-1's suggestion, not required for this CLI fix.

## Diff
crates/caco-cli/src/lib.rs. Final landed squash SHA per the reintegration receipt.

## SPEC areas
- SPEC 18 (beads CRUD) / agent coordination: aligns claim/unclaim 503/no-payload
  responses with the existing indeterminate-claim verify-first contract
  (AGENTS.md indeterminate_claim/indeterminate_create), preventing over-claiming.
