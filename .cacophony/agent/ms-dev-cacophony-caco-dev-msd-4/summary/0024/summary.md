# Summary 0024 — bd-845653: harvest_branch_bead_ids fallback to direct log

## Bead
bd-845653 (P2, bug, reopened+reclaimed) — "Reintegrate squash
commit message footer omits the bead ID present in the agent
commit subject (bd-58ff27 lost)."

Bead was previously closed by msd-2 but root cause unfixed —
msd-2 reported 14 stuck-open beads from the same harvest-empty
shape this session.

## Root cause

`harvest_branch_bead_ids(checkout, target, agent_branch)` runs
`git log --format=%B {target}..{agent_branch}` in the canonical
reintegration checkout. When the range is empty (already-merged
branch, stale local ref, post-rebase identical-tree, etc.), it
returns `Vec::new()`.

Caller `build_reintegration_commit_message_with_branch_ids` then
hits this match arm:

    match branch_bead_ids {
        Some(branch_ids) if !branch_ids.is_empty() => filter+merge,
        _ => goal_details,   // <-- noise pollution path
    }

so a `Some(empty)` falls back to legacy goal-text extraction.
Persistent agents have multi-KB goal blobs that mention many
example bead IDs in boilerplate instructions; those flood the
squash-commit footer instead of the agent's actual claimed bead
(reproduction in msd-2's bd-845653 description: footer carried
6 unrelated IDs across multiple back-to-back reintegrates while
the actual bd-58ff27 was nowhere to be found).

Downstream consequence: `caco bd close --validate-on-main`
rejects the close because the actual bead ID isn't in the squash
footer. Stuck-in-progress beads accumulate.

## Change

`crates/caco-daemon/src/reintegration.rs::harvest_branch_bead_ids`:

- When the `{target}..{branch}` range query succeeds but returns
  zero IDs, fall through to a direct `git log --format=%B -20
  {agent_branch}` scan of the agent branch's recent commits.
- Bounded to 20 commits — enough to find the bead ID for the
  current cycle without walking full project history.
- Preserves all-error → empty-vec semantics for the caller's
  legacy goal-fallback to keep working when even the direct scan
  fails (e.g. the branch ref is genuinely missing).

This means in the msd-2 reproduction shape (range empty because
agent branch already merged into target via prior cycle), the
direct-log path will still find `bd-58ff27` on the agent branch
HEAD's commit message and emit it in the footer.

### Tests

`crates/caco-daemon/src/reintegration.rs::tests`:

- `harvest_branch_bead_ids_falls_back_to_direct_log_when_range_empty`:
  reproduces msd-2's exact shape — create agent branch with
  `bd-58ff27` commit subject, fast-forward into main so the
  range becomes empty, assert the harvest still finds bd-58ff27
  via the direct-log fallback.
- `harvest_branch_bead_ids_uses_range_when_non_empty`: back-
  compat — when range is non-empty, the existing path runs
  and returns its IDs. (Implicit: the fallback isn't triggered
  for the ordinary case; this test fixes the contract.)

## Drive-by

`crates/caco-daemon/src/lib.rs` — 4× test-fixture
`agent::AgentInfo { ... }` literals missing `annotation: None,`
field (post-bd-4a9bf4 wave). Added in test sites only:
3× `completed-retention-plan` cousins + 1× cleanup test.
Pre-existing peer landing of `dispatch_operator_actions_list`
(an unrelated bd-6b7b30 attempt) is dead code with a pre-
existing clippy warning — left alone, not in scope here.

## Verification

- `cargo test -p caco-daemon --lib harvest_branch_bead_ids`:
  2/2 pass.
- `cargo test-small`: all green (~4255 tests).
- `cargo clippy --workspace --no-deps`: only the pre-existing
  `dispatch_operator_actions_list` dead-code warning (not from
  this change).

## Operational impact

Persistent agents that reintegrate an already-merged-equivalent
or post-rebase branch will now correctly attribute their work in
the squash-commit footer. The 14-stuck-open bead pattern msd-2
reported should stop accumulating after this lands and the
daemon binary is rolled forward.

Existing close-validator bypass (`--validate-on-main false` from
msd-2 + my session uses) remains the recovery tool for already-
landed-but-unattributed beads. This patch removes the source.

## Companion / related

- bd-cbbc22 (existing): goal-text harvest path.
- bd-da8803 (existing): full bead title/desc payloads.
- bd-58ff27 (msd-2 reproduction; closed as duplicate of this
  bead in spirit).
- bd-845653's other follow-ups (bd-f76c81 / bd-6da2c1 — separate
  bug class, post-verify race) — already landed.

## Next

Reintegrate, close bd-845653, idle. Speak the fix to the fleet
since it directly improves msd-2's daily friction.
