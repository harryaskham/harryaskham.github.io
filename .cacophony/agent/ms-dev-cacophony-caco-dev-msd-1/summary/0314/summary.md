# Session summary — bd-e8dfe7 part-2: suppress the spurious dead-letter for manual-checkout reintegrate refusals

## Goal
Complete bd-e8dfe7's "failed attempts give actionable guidance instead of repeated bd-4b1ffd dead
letters" acceptance: when an operator runs `caco agent reintegrate` from a manual
`caco project checkout` (`manual/project_checkout/<project>`) branch, the bd-4b1ffd refusal is a
USER ERROR (the commit is safe on the manual branch), not a stranded reintegration — so it must NOT
persist a spurious `reintegration_dead_letters` row. part-1 (the actionable message) already landed;
this is part-2 (the dead-letter suppression), per msm-2's complete fix-map.

## Bead(s)
- bd-e8dfe7 (P2 task). part-1 (message) + part-2 (this) complete the actionable-guidance acceptance.
  The MAIN feature (the ergonomic `caco project reintegrate --checkout` land path, msm-3's Option A)
  is filed as bd-1d2935 (filed FIRST, per the auto-close-landed lesson — this commit footers
  bd-e8dfe7 so the landed-sweep auto-closes it).

## Before state
The manual-checkout refusal message landed (part-1), but the refusal still flowed to
`classify_reintegration_dead_letter` → fell through to `reintegration_failed` → persisted a
spurious dead-letter on every manual-checkout reintegrate attempt (the failure-classification path,
caco-cli, not reintegration.rs — corrected msm-2's verified flow).

## After state
- Shared marker const `caco_daemon::reintegration::MANUAL_CHECKOUT_REINTEGRATION_REFUSAL_MARKER`
  ("this is a branch-selection error, not lost work"), embedded in
  `agent_branch_checkout_mismatch_message`'s manual-checkout branch (the phrase was already there —
  now referenced via the const so producer + consumer can't drift on a fragile substring).
- caco-cli `classify_reintegration_dead_letter`: early `return None` guard gains
  `|| is_reintegration_manual_checkout_branch_mismatch(outcome)` + the helper (mirrors the existing
  is_ordinary_stale_reintegration_retry skip), keyed on the shared const.
- 1 unit test (manual-checkout marker → None/skipped; generic failure → Some/captured).
- Read-only classifier behavior change: no false-positive dead-letter for the user-error case; a
  genuine reintegration failure still captures a dead-letter. No land-path/publish behavior change.

## Diff summary
- crates/caco-daemon/src/reintegration.rs: MANUAL_CHECKOUT_REINTEGRATION_REFUSAL_MARKER const +
  reference it in agent_branch_checkout_mismatch_message.
- crates/caco-cli/src/lib.rs: classify guard + is_reintegration_manual_checkout_branch_mismatch
  helper + 1 unit test.
(Final landed squash SHA: see the reintegration receipt.)

## Operator takeaway
Landing a config edit from a manual `caco project checkout` via `caco agent reintegrate` (the
wrong-branch user error) no longer spams `reintegration_dead_letters` — it gets the actionable
message (part-1) and is correctly classified as a non-stranding user error (part-2). The ergonomic
one-command land path itself (the bead's MAIN acceptance) is the bd-1d2935 follow-on (msm-3's
Option A design).
