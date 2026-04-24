# Session summary — Refuse-on-dirty preflight closes silent data-loss in `caco agent reintegrate` / `complete`

## Goal

Close the silent-data-loss path filed in bd-ab3050 (P1 bug, data-loss
label): when an agent calls `caco agent reintegrate` (or
`caco agent complete`) with uncommitted changes in its worker
checkout, the reintegration flow can return success while
`sync_checkout_to_target` then `git reset --hard origin/<target>`
silently wipes the uncommitted work into the reflog with no commit
anywhere on any branch.

## Bead(s)

- `bd-ab3050` — caco_agent_reintegrate silently wipes uncommitted
  changes via implicit reset --hard origin/main

## Before state

- `dispatch_agent_reintegrate` and `dispatch_agent_complete` in
  `crates/caco-cli/src/lib.rs` accepted any worker checkout state
  (clean or dirty).
- The post-success `sync_checkout_to_target` performed an
  unconditional `git reset --hard origin/<target>` on the worker
  checkout.
- A successful "reconciled: skipped redundant merge" outcome combined
  with a dirty worktree silently wiped uncommitted work; tool returned
  `success: true`; reflog was the only trace.
- `git rebase` already refused-on-dirty; `reintegrate` did not.

## After state

- New helper `refuse_reintegrate_when_worktree_dirty` runs
  `git status --porcelain -uall` on the worker checkout and returns
  a loud, actionable error if the worktree is not clean.
- Wired into both `dispatch_agent_reintegrate` and
  `dispatch_agent_complete` immediately before the
  `ReintegrationRequest` is built.
- Cross-project reintegration (`--project` + `--checkout`) keeps the
  legacy behaviour because the externally-managed checkout is the
  caller's responsibility.
- Error message gives the agent the exact recovery recipe.

## Diff summary

- Commit: `5e5ada186 bd-ab3050: refuse caco agent reintegrate/complete
  when worktree is dirty`
- Files touched: `crates/caco-cli/src/lib.rs` (+150 lines including 4
  new unit tests)
- Tests: +4 (refuse_..._clean_returns_ok,
  refuse_..._modified_returns_err,
  refuse_..._untracked_file_returns_err,
  refuse_..._staged_change_returns_err) — all pass.
- Behavioural delta: any agent invoking `caco agent reintegrate` or
  `caco agent complete` with a dirty worktree (modified, staged, OR
  untracked) now gets a refusal mentioning bd-ab3050 and a precise
  recovery recipe instead of silent data loss.

## Operator-takeaway

The bd-ab3050 silent-wipe path is closed at the most defensive layer:
the CLI dispatcher refuses BEFORE the daemon's reintegration request
is even built. Post-success `sync_checkout_to_target` is left intact
for the now-guaranteed-clean case. Follow-up hardening (auto-commit
"[reintegrate fallback commit]" or making the "reconciled: skipped
redundant merge" path explicitly warn before the reset) is deliberately
left as a separate bead if the symptom is observed again — this fix
alone removes the entire reported reproducer.
