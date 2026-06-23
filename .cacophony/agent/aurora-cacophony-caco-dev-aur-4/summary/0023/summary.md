# bd-c2d9a5 (gap 1): classify backfill-stranded transport timeout as indeterminate

## Problem
A large `caco summaries backfill-stranded` (verified 3328-path ms-dev recovery)
runs minutes in the daemon's spawn_blocking (rebuild + commit + SSH-push of the
whole stranded set) and can outlive even the 600s request timeout (raised from
20s by e50451f0b5) AND SUCCEED. The CLI rendered the resulting transport
timeout/error as a plain "backfill-stranded failed" — a FALSE-NEGATIVE an
operator could blind-retry (double-push risk).

## Fix (gap 1 — mirrors bd-d75625 / bd-6bfb19 indeterminate-apply class)
- `backfill_transport_is_indeterminate(value)`: ok=false + http_status 503/504 OR
  code in {summaries_unavailable, transport_error, empty_response,
  daemon_restarting}.
- In dispatch_summary_backfill_stranded_impl, for a NON-dry-run backfill, classify
  a transport timeout/error as INDETERMINATE (json envelope + text Err) and return
  verify-first guidance (`backfill_indeterminate_outcome` / `_message`): "MAY have
  completed + pushed server-side; VERIFY the forge / --dry-run; do NOT blind-retry
  -> could double-push" (retryable:false, verify_first:true). Dry-run timeouts stay
  plain failures (no push).

## Validation
- cargo check -p caco-cli --tests green (2m33s; only a pre-existing unrelated
  caco-daemon warning).
- cargo check --workspace --tests (cross-crate, per ctrl's gate-equivalent).
- Unit test backfill_transport_is_indeterminate_bd_c2d9a5 (truth table); queued.

## Scope / credits
ctrl-approved split. Filed by the bd-5804a1 follow-up; gap-1 impl+test aur-4.
GAP 2 (daemon-side async/poll endpoint per the bead's option (a) + tree-build
stress profiling/chunking) is the durable follow-up — re-scope bd-c2d9a5 to it or
child-bead it.

## Diff
crates/caco-cli/src/summary_cmd.rs. Final landed squash SHA per the reintegration
receipt.

## SPEC areas
- SPEC 18 / cacophony-state recovery: aligns backfill-stranded transport failures
  with the indeterminate-mutation verify-first contract (bd-d75625, AGENTS.md
  bd-5804a1 backfill-stranded), preventing double-push from blind-retry.
