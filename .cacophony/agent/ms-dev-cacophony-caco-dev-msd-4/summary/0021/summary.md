# Summary 0021 — bd-274c2d: test-health cycle (clean) + post-mortem

## Bead
bd-274c2d (P1, permanent, claimed) — continuous test-suite health.

## Cycle results
- `cargo test-small`: 4247/4247 (post msd-1's bd-91a14c slice 1 +
  the AttachMetadata/SessionKickedModal/Bead fixture re-fixes).
- `cargo clippy --workspace --no-deps`: zero warnings.
- All previously-watched flakes still in their expected state.

## Post-mortem of summary 0020 reintegrate failure

Summary 0020's reintegrate failed twice with
`bd-8b1492: ... no merge commit was recorded`. Root cause: my agent
branch had silently reverted msd-2's bd-fac12a ECHILD-recovery
edit in `crates/caco-beads/src/store.rs::run_git_with_retry`. This
happened during the previous bundle-rebase when a conflict was
"empty"-resolved via `git rebase --continue` — the resolution kept
my older pre-msd-2 view of the file, which on push appears as a
revert of msd-2's bd-fac12a allowlist additions.

Combined effect: my agent branch had non-artefact diff (the
unintended store.rs revert) PLUS the artefact summary file. The
squash-merge produced a non-empty merge commit, but apparently the
post-verification then saw `merge_commit: None` (likely because the
unintended revert created a no-op-after-rebase-detection edge case).

Recovery: `git reset --hard origin/main` to drop the silent revert,
then re-claim and re-do this cycle.

## Lesson learned (operational)

After ANY rebase that reports an "empty" preimage / commit-already-
applied resolution, run `git diff origin/main HEAD --stat` BEFORE
push/reintegrate. If non-summary files appear, abort + investigate
— the rebase likely silently dropped someone else's recently-landed
work into your version. This is the pattern that bit me twice this
session (race-loss visible only by inspecting full diff).

Filing as bd follow-up: pre-reintegrate hook should diff agent-
branch HEAD vs origin/main, and reject if non-artefact paths
contain reverts (lines REMOVED that exist on origin/main HEAD but
NOT on the merge-base).

## Next

Reintegrate this clean summary, unclaim bd-274c2d, file the rebase-
diff-guard follow-up, idle.
