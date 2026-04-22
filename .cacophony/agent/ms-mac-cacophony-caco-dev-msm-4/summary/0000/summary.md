# bd-d69565 — caco agent rebase auto-pushes rebased tip

## Goal
Eliminate the manual `git push --force-with-lease` step that operators
have been hitting whenever `caco agent rebase` is followed by
`caco agent reintegrate`.

## Bead(s)
- bd-d69565 (P2 bug) — caco agent reintegrate's auto-rebase fights
  with concurrent agent-branch updates: needed manual force-with-lease
  tonight. Pairs with bd-a51211 (server-side non-FF auto-recovery,
  already landed) and bd-732406 (daemon-restart re-attach, separate).

## Before state
- `caco agent rebase` ran `git fetch origin <target>` + `git rebase
  origin/<target>` and stopped there. The local agent branch advanced;
  the remote one did not.
- The next `caco agent reintegrate` then tried to push the rebased
  branch back to its own remote ref and was rejected non-fast-forward
  (the remote still held the pre-rebase commit).
- Operators worked around by running `git push --force-with-lease`
  manually before re-running reintegrate.
- bd-a51211 added a single-shot auto-recovery to the reintegration
  flow's `push_agent_branch`, but the rebase command itself never
  pushed at all, so an extra round-trip was always required.

## After state
- After a successful local rebase, `dispatch_agent_rebase` now runs
  `git push --force-with-lease origin <agent_branch>:<agent_branch>`.
- Agent branches are single-writer (the agent process itself), so
  `--force-with-lease` is safe: it only succeeds when the remote tip
  matches what we last observed and refuses if a concurrent writer
  has advanced it.
- The push is best-effort. On failure we annotate the success message
  with a `warning: bd-d69565 …` line but do NOT mask the rebase
  success — operators can still re-run reintegrate, which routes
  through bd-a51211's auto-recovery on its push attempt.
- JSON mode surfaces the same warning under
  `"post_rebase_push_warning"` so automation can decide whether to
  retry or proceed.

## Diff summary
- `crates/caco-cli/src/lib.rs` (`dispatch_agent_rebase`):
  - After the rebase succeeds, run a `git push --force-with-lease`
    against the resolved agent branch.
  - Capture push failures into an optional warning string.
  - Plumb the warning into both the JSON envelope and the human-readable
    success message.

## Operator-takeaway
The two-command `caco agent rebase` → `caco agent reintegrate` flow
should now work first time without an interleaved manual force-push.
If you ever see a `bd-d69565 post-rebase push failed` warning, just
re-run `caco agent reintegrate` — bd-a51211's auto-recovery will pick
up from there.

## Tests
- `cargo test -p caco-cli --lib agent_rebase` — 1 passed (the existing
  help-text test). The push branch is best-effort and would require a
  full live-git fixture to exercise meaningfully; the contract is
  asserted by the manual workflow this bead is closing.
