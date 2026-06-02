# Session summary — bd-a08d6e: resolve reintegration target through stale mirror

## Goal

Fix a P1 root cause where reintegration's receipt target resolution
(target_before / target_after) reads the agent checkout's daemon canonical
MIRROR origin instead of the authoritative GitHub upstream. When that mirror's
fetch loop is wedged/stale (the bd-b1ce01 / bd-e3b10f class), the stale ref
drives a spurious bd-4b1ffd stale-branch rejection of a correctly-rebased
branch — the msd-1 / bd-257067 reintegration-block class observed on ms-dev.

## Bead(s)

- `bd-a08d6e` — Reintegration target_before/bd-4b1ffd must resolve authoritative
  upstream, not a stale daemon mirror. P1 bug.
- Filed `bd-9f33ab` (P1 broken-on-main) — 17 pre-existing reintegration::tests
  direct_mode failures discovered while validating (NOT caused by this change).

## Before state

- prepare_direct_attempt_receipt / finish_direct_attempt_receipt called
  remote_branch_head(req.checkout, req.remote, ...) which ls-remotes the agent
  origin = the daemon mirror, so a stale mirror yielded a stale target_before.
- Failing tests: 17 pre-existing reintegration::tests failures on clean main
  (unrelated, filed as bd-9f33ab).

## After state

- New authoritative_remote_branch_head reuses the tested
  resolve_canonical_checkout_target helper to ls-remote the mirror's own origin
  (the forge) when remote is a local non-bare mirror; falls back to the direct
  read when origin is the forge. The two receipt sites now use it.
- Failing tests from THIS change: none. +2 new unit tests green; clippy
  -p caco-daemon --lib clean. The 17 pre-existing failures are unchanged
  (bd-9f33ab).

## Diff summary

- Code commit: pending final squash SHA from the reintegration receipt.
- Files touched: crates/caco-daemon/src/reintegration.rs (+146/-2).
- Tests: +2 (stale-mirror upstream resolution; bare-origin fallback).
- Behavioural delta: receipt target_before/target_after now reflect the true
  upstream tip even when the local mirror is stale, removing one source of
  spurious bd-4b1ffd rejections.

## Embedded artefacts

- none.

## Operator-takeaway

The live bd-4b1ffd REJECTION path already resolved authoritatively (preflight +
isolated-integration-checkout both go mirror->upstream via bd-e44543), so the
residual gap was specifically the receipt's informational target_before/after
reading the stale mirror directly — which is the exact ref msd-1 observed. This
fix closes that last mirror-trusting read by reusing the existing canonical
resolver, no new parallel logic. Separately, the whole direct_mode reintegration
test suite is currently broken-on-main (bd-9f33ab) for an unrelated reason worth
a dedicated fix.
