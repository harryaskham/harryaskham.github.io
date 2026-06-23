# Session summary — bd-769727 cacophony-state publish non-ff bounded retry

## Goal

Overnight spike burn-down: harden the cacophony-state (summary/sidecar) branch publish against the non-fast-forward contention that, under high fleet concurrency, leaves sidecars stuck in the state-outbox. This is the state-branch analogue of the bd-0ec380 main-push race the fleet was actively guarding against this session.

## Bead(s)

- `bd-769727` — [data-integrity] cacophony-state branch contention: sidecar/summary publish non-ff races under high fleet activity (P3, non-blocking). Related: bd-0ec380 (main-push race), bd-7a32e0 (in-queue agent-branch non-ff retry, the pattern mirrored here).

## Before state

- Failing tests: none. `publish_state_outbox_ref` (cacophony_state.rs) handled a non-ff push rejection with a SINGLE recovery attempt: fetch latest cacophony-state tip -> replay the pending commit onto it -> push. Under sustained concurrency another node could advance the branch between that attempt's fetch and push, so the recovery push lost the race again and failed, returning accepted_with_warning and leaving the sidecar in the outbox (the bead's reported symptom).

## After state

- Failing tests: none. The non-ff recovery is now a bounded jittered RETRY LOOP: on each non-ff push loss it re-fetches an even-newer tip, replays onto it, and re-pushes, up to `CACO_STATE_PUBLISH_NONFF_MAX_RETRIES` (default 5) attempts with a 2s-bounded jittered backoff, so it converges under contention instead of growing the outbox. A REAL artefact-path replay conflict still drops immediately (propagated, not retried); a non-contention push failure (transport/auth) still surfaces; the throwaway rebased ref is cleaned between attempts. Behavior-preserving for the common single-attempt success path.
- Validation (queued, green): `cargo test -p caco-daemon --lib state_publish_nonff` 1/1 pass (new `state_publish_nonff_retry_tuning_bd_769727`: parse floor/default + bounded monotonic backoff); `cargo clippy -p caco-daemon` clean; rustfmt-clean hunks.

## Diff summary

- Code commit: pre-final-squash; final landed squash SHA from the reintegration receipt.
- File: `crates/caco-daemon/src/cacophony_state.rs` (env-tunable bounded-retry constants + 4 pure/IO helpers + the retry-loop refactor of publish_state_outbox_ref's non-ff arm + 1 test).
- Tests: +1; behavioural delta: state-branch publish now bounded-retries non-ff contention to convergence instead of one-shot-failing into the outbox.

## Operator-takeaway

The cacophony-state sidecar/summary publish now survives sustained cross-node non-ff contention by bounded-retrying fetch->replay->push (default 5, jittered, 2s-capped) — the state-branch counterpart of the bd-7a32e0 agent-branch retry. This reduces state-outbox growth + accepted_with_warning churn during busy windows. NOTE: the non-ff bounded-retry pattern now exists in two near-identical forms (bd-7a32e0 reintegration.rs for the agent branch, bd-769727 cacophony_state.rs for the state branch) — a shared bounded-retry util is a reasonable future consolidation (filed as a reflect-draft). The durable cross-branch fix remains PR-mode (bd-c26699), which bypasses the local-mirror push races entirely.
