# Session summary — protect operator-decision beads from generic auto-claim (bd-0889db)

## Goal

Generic no-ID auto-claim skips `operator-action`-labeled / `[operator-action]`-titled
beads, but NOT `operator-decision` ones. So an operator-DECISION bead (an operator-gated
posture/design call, e.g. Harry's gate-disable reversal bd-ff92cd) could be mis-claimed
by a generic idle worker, burning cycles on operator-gated work (bd-ff92cd was claimed by
a worker that did no work + failed on tmux). Treat operator-decision like operator-action
for claim-protection.

## Bead(s)

- `bd-0889db` — [auto-claim] operator-decision-labeled beads are not auto-claim-skipped
  (only operator-action) — bd-ff92cd mis-claimed by a worker.

## Before state

- Failing tests: none.
- `Bead::requires_explicit_assignment` (caco-beads model.rs) checked `[operator-action]`
  title prefix + `operator-action` label only. `is_generic_ready_queue_candidate` (the
  auto-claim candidate gate) calls `!requires_explicit_assignment()`, so operator-decision
  beads were generic-auto-claim-eligible.

## After state

- Failing tests: none.
- `requires_explicit_assignment` now also matches `[operator-decision]` title prefix +
  `operator-decision` label, so they are excluded from no-ID auto-claim (propagates to
  `is_generic_ready_queue_candidate` automatically). The no-ready hint text lists
  operator-decision too. Explicit `--bead-id` claims by an operator/controller are
  unaffected (only generic no-ID auto-claim is gated).

## Diff summary

- Files: `crates/caco-beads/src/model.rs` (predicate + test), `crates/caco-daemon/src/beads.rs` (hint text).
- Tests: extended `generic_ready_queue_candidate_excludes_permanent_epic_and_operator_action`
  with operator-decision coverage (both title-prefix + label forms). GREEN.
- Validation: cargo test -p caco-beads --lib generic_ready_queue_candidate GREEN;
  cargo check --workspace --tests on rebased content.
- Final landed squash SHA from the reintegration receipt.

## Operator-takeaway

operator-decision beads (operator-gated posture/design calls) are now protected from
generic idle-worker auto-claim the same way operator-action beads are, so a worker can't
mis-claim an operator's decision bead and burn cycles. Closes the bd-ff92cd mis-claim
class (which was manually protected by adding operator-action).
