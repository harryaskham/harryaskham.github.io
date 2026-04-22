# Session summary — bd-f76c81 reintegrate accepts artefact-only success

## Goal

Stop `caco agent reintegrate --mode direct,recorded` from failing with `bd-8b1492: ... no merge commit was recorded` when the agent branch contains only artefact files (e.g. session summaries from observation-only / triage cycles).

## Bead(s)

- `bd-f76c81` — Reintegrate fails post-verification when agent branch is summary-only (no code diff).

## Before state

- `finalize_direct_merge` correctly skips creating a `target` commit when all staged paths get stripped into the cacophony-state artefact split. `outcome.success = true`, `outcome.merge_commit = None`.
- `verify_direct_outcome` treated `None merge_commit` as a false-success and returned an error.
- The wrapper in `reintegrate()` then rewrote the outcome to `success: false` with the verbose `bd-8b1492` message, and the CLI surfaced that to the operator.
- The permanent test-health bead bd-274c2d / triage cycles were forced to skip reintegration entirely or fold a no-op code change in.

## After state

- `ReintegrationOutcome` gains `artefact_commit: Option<String>` (serde-skipped when `None`).
- The 24 construction sites in `crates/caco-daemon/src/reintegration.rs` and `crates/caco-daemon/src/agent/tests.rs` set `artefact_commit: None` by default; `finalize_direct_merge` populates it from `artefact_split.commit` on the success path.
- `verify_direct_outcome` returns `Ok(())` when `merge_commit.is_none()` AND `artefact_commit.is_some()` — the legitimate artefact-only path. The pre-existing rejection still fires for `None + None` (true false-success).
- New regression test `verify_direct_outcome_accepts_artefact_only_success` pins the artefact-only contract.
- Existing test `verify_direct_outcome_rejects_missing_merge_commit` still rejects `None + None` because it does not set `artefact_commit`.

## Diff summary

- Commits: `5b6b2dea`.
- Files touched: `crates/caco-daemon/src/reintegration.rs`, `crates/caco-daemon/src/agent/tests.rs` (+86 lines net).
- Tests: 105 reintegration tests pass (added 1: 104 → 105).
- Lints: `cargo clippy -p caco-daemon --all-targets -- -D warnings` clean.
- Schema delta: `ReintegrationOutcome` JSON now optionally includes `artefact_commit`. Skipped when `None`, so no break for existing consumers parsing the older shape.

## Operator-takeaway

Triage / observation-only cycles can now reintegrate cleanly under `--mode direct,recorded`, which was the original purpose of the recorded mixin. The CLI's user-facing error message stays unchanged for the genuine false-success case so we don't regress the bd-8b1492 protection.
