# Session summary — bd-0ffc21 (a-refined): the reintegration wedge/livelock keystone fix

## Goal

Fix bd-0ffc21 — the reintegration gate-under-lock wedge that caused a staleness-livelock: a slow (~44min) reintegration gate held the merge-queue `runner.lock` across its queue-wait, so other reintegrations blocked long enough that `main` drifted past `reintegration_max_stale_count` and they stale-rejected repeatedly, producing the dead-letter bursts and contention that OOM'd ms-dev. The approved design (ms-dev-ctrl: "a-refined + fast-disjoint-check") runs the gate OUTSIDE the lock (phase-1) and re-validates under the lock (phase-2) only when the target moved.

## Bead(s)

- `bd-0ffc21` — [reintegration-reliability] Reint gate stuck in saturated test queue holds the reint lock, blocking other lands (the keystone; unblocks Slice A + clears the reint-livelock/dead-letter bursts).

## Before state

- `run_serialized_direct` (merge_queue.rs) held the project `runner.lock` across the WHOLE reintegrate closure, including `finalize_direct_merge`'s gate (a queue-backed `caco test run --gate` that waits in a saturated queue). Lock held during the wait -> second reintegration blocks -> main drifts -> stale-reject -> livelock.
- No phase-1/phase-2 split; the gate always ran under the lock.

## After state

- Gate runs OUTSIDE the lock; the lock is held only for the brief publish phase; moved-tip re-validation preserves frozen-tip atomicity. All existing reintegration tests pass (None-path byte-identical) + 8 new bd-0ffc21 tests green + `cargo check --workspace` green + test-small fully-clean green.

## Diff summary

- Code commits (agent branch, squashed on land — final SHA from the reintegration receipt): BB1 classifier (already landed ba16b9a30) + BB2 phase-2 decision + BB3 FastCheck executor + step-i (finalize consumes phase2_gate_decision) + step-ii-a (thread phase1_gated_tip) + step-ii-b (run_phase1_gate outside the lock + reintegrate() restructure) + step-iii (8 tests + the run_phase1_gate_in_checkout refactor).
- Files: `crates/caco-daemon/src/reintegration.rs` (run_phase1_gate, run_phase1_gate_in_checkout, phase2_gate_decision, classify_target_advance, the reintegrate() restructure, finalize phase-2 consumption, 8 tests), `crates/caco-daemon/src/reintegration_gate.rs` (run_reintegration_fast_check).
- Behavioral delta: a slow gate no longer holds the runner.lock (wedge fixed); the queue still serializes the gate runs (no oversubscription). Phase-2: tip unchanged -> Skip (publish the validated tree); tip moved -> Regate (path-overlap, full gate) / FastCheck (path-disjoint, cargo check --workspace); conflict/empty/no-gate -> byte-identical in-lock fallback.
- Tests: +8 bd-0ffc21 (classify x2, phase2_gate_decision, finalize moved-tip re-gate, conflict->None, clean->Some+frozen-tip, no-gate/on-main guards).

## Embedded artefacts

None (daemon-internal logic; no visual surface).

## Operator-takeaway

The single most important correctness invariant is FROZEN-TIP ATOMICITY: the tree finalize publishes is ALWAYS the tree it validated. Same-tip between gate and lock -> the deterministic squash is byte-identical to the gated tree (Skip is safe); moved-tip -> phase-2 re-gates/fast-checks the NEW merge against the current tip before push. The end-to-end `finalize_moved_tip_regates_not_skips` test proves finalize never publishes an ungated tree on a moved tip (a wrong Skip would publish a failing gate; the correct re-gate rejects it). This keystone clears the reint-livelock/dead-letter contention that drove the redundant-reintegration churn and the ms-dev OOM.
