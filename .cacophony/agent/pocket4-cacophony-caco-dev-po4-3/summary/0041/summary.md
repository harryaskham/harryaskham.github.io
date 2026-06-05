# Summary — bd-6d2616 reap stale index.lock before dirty-work checkpoint

## Goal
bd-6d2616 (P4, agent-infra/git-lock): a recurring 0-byte `.git/index.lock` left
behind by a prior git/submodule op persists past the daemon's stale threshold
(observed 335s/370s/1068s/2604s) with no holder and no active git, then blocks
the next git mutation. Implement the bead's follow-up #2: let a direct daemon
commit path optionally reap a confirmed-stale lock the way `caco agent rebase`
already does.

## Root cause / gap
`checkpoint_dirty_work` (crates/caco-daemon/src/agent/health.rs) — the
WIP-preservation commit on involuntary stop — ran `git add -A` + `git commit`
with **no** stale-lock guard, unlike reintegration / checkout refresh /
`caco agent rebase`, which already call `ensure_git_index_lock_not_blocking` /
the `git_lock_cleanup` predicate. A stale lock there made the checkpoint fail
and the agent's uncommitted work was **lost** — the worst case the observation
describes.

## Fix
Before staging, best-effort reap a confirmed-stale lock via
`cleanup_stale_git_index_lock(checkout, infer_managed_checkout_roots(checkout))`.
This reuses the existing safe predicate (lock under a managed checkout root,
age past `STALE_GIT_LOCK_MIN_AGE` = 5min, no lsof holder, no active git cwd'd in
the checkout). It is **non-fatal**: a genuinely-held or fresh lock is preserved
and the subsequent commit surfaces the real error; only a `Removed` action is
logged. No change to the shared predicate, so reintegration/rebase behavior is
untouched.

## Test
`checkpoint_dirty_work_reaps_stale_index_lock_bd_6d2616` builds a managed temp
checkout (path under `.cacophony/daemon/checkouts/...` so it is recognized as
managed without env mutation — parallel-safe), makes it dirty, drops a 0-byte
`index.lock` with an old mtime, and asserts the checkpoint succeeds, the lock is
removed, and the WIP is committed (checkout clean afterwards). Green via the
16MB-stack daemon lib lane.

## Scope / remaining
Targeted the highest-value unguarded commit path (WIP loss on stop). The bead's
follow-up #1 (whether the daemon's own recursive submodule op leaks the lock and
could clean up on completion) is a separate, deeper investigation not in this
slice; the worker workaround remains reliable and this fix removes the WIP-loss
risk on the checkpoint path. Headless-verifiable (daemon Rust logic, no visual).

## Diff
See the landed squash commit in the reintegration receipt (code commit
`ef19c6043` touching `crates/caco-daemon/src/agent/health.rs`).
