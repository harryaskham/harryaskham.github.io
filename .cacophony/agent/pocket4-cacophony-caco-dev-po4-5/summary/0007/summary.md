# Session summary — Stop false DESTRUCTIVE markers on envelope compaction

## Goal

Close bd-0c1994 (P1): the reconciler's destructive-write detector
falsely flagged routine envelope compaction as `⚠ DESTRUCTIVE`,
producing alarm fatigue identical to the bd-96dcf5 dedup case.

## Bead(s)

- `bd-0c1994` — Fix false DESTRUCTIVE markers for beads reconcile
  mutation-envelope compaction

## Before state

- `extract_ids()` in `crates/caco-beads/src/store.rs` read the
  top-level `"id":"..."` field from every JSONL row.
- Worked for snapshot rows (`id` = `bd-XXXXXX`), but mutation
  envelopes (`id` = `mut-...`) and export envelopes
  (`id` = `export-bd-...`) keep the logical bead in `bead_id`.
- Routine envelope compaction (mut-/export- folded into snapshot row)
  appeared to "lose" the envelope ids, tripping the DESTRUCTIVE
  prefix on commits like ms-mac's 26e0c7b4c2 / c7b99a0f0c.

## After state

- `extract_ids()` prefers `bead_id` when present (envelope rows),
  and only accepts top-level `id` when it starts with `bd-`
  (snapshot rows).
- Set-difference now compares LOGICAL bead sets, so envelope
  compaction is correctly classified as benign.
- New test
  `reconcile_envelope_compaction_is_not_destructive_bd_0c1994`
  seeds the exact failure scenario (snapshot + mutation envelope
  + export envelope, all referencing one bead) and asserts the
  commit message does not carry DESTRUCTIVE.

## Diff summary

- Commit: bd-0c1994 stop false DESTRUCTIVE markers
- File: `crates/caco-beads/src/store.rs` (+87/-8)
- Tests: all 17 reconcile_* tests pass; cargo test-small green.

## Operator-takeaway

`⚠ DESTRUCTIVE bd: sync reconcile` in `caco bd log` / git log on the
beads worktree now reliably indicates a real loss of logical bead
ids, not envelope-row churn. Past commits showing the prefix during
routine peer compaction were false alarms — the underlying beads are
intact (operator-verified via `caco bd show` for the four named
beads in the report).
