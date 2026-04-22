# Session summary — allow respawn after Failed/Stopped previous worker

## Goal

Unstrand permanent beads (and any bead in general) whose previous
worker died into `Failed` or `Stopped` after a daemon outage. The
duplicate-worker guard added in bd-13ae4f was over-broad and refused
the new spawn even though the bead had never been successfully
completed.

## Bead(s)

- `bd-290598` — Permanent bead dispatcher rejects respawn when previous worker is in failed state — bd-1c0bdd stuck after daemon outage

## Before state

- `find_any_agent_for_bead` returned every non-`Discarded` agent assigned to the bead.
- Spawn path used that result as a hard "duplicate worker" rejection.
- Effect: a `Failed`/`Stopped` corpse blocked dispatch indefinitely. Operator workaround was unknown — `bd update --assignee ""` did not help because the rejection check inspects the agent table, not the bead's assignee field.
- Failing tests: none (the pre-existing tests only cover the `Completed` race case).

## After state

- `find_any_agent_for_bead` now skips `Discarded` **and** `Failed` **and** `Stopped`. `Completed` still blocks — that's the bd-13ae4f race window between agent-completion and bead-closure, which is real and unchanged.
- Three new tests pin the new behaviour:
  - `find_any_agent_for_bead_skips_failed_so_respawn_is_allowed`
  - `find_any_agent_for_bead_skips_stopped_so_respawn_is_allowed`
  - `find_any_agent_for_bead_still_blocks_on_completed` (regression guard for bd-13ae4f)
- The three pre-existing tests still pass unchanged.
- Comment at the spawn callsite updated to reference bd-290598 and explain the carve-out.
- `cargo build -p caco-daemon` clean. `cargo clippy -p caco-daemon --lib` clean. All 6 `find_any_agent_for_bead` tests pass.

## Diff summary

- Commits: `61a3a992`
- Files touched: `crates/caco-daemon/src/agent/lifecycle.rs`, `crates/caco-daemon/src/agent/tests.rs`, `crates/caco-daemon/src/lib.rs`
- Tests: +3 (Failed allowed, Stopped allowed, Completed still blocks)
- Behavioural delta: dispatching a worker against a bead whose previous worker is `Failed` or `Stopped` now succeeds. Permanent beads recover automatically after daemon outages instead of needing operator intervention.

## Operator-takeaway

If you have permanent beads currently stuck behind a `Failed` worker (e.g. bd-1c0bdd referenced in the bug report), they will be redispatched on the next reconcile cycle once this lands. No manual `bd update --assignee ""` or `caco agent discard` is needed. Workers that legitimately Completed still block respawn until the bead is closed by reconcile — bd-13ae4f's race-window guard is intact.
