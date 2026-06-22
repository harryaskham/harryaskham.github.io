# Session summary — bd-be6d54: file-cache sync uses the shared remote (not the node-local mirror)

## Goal
Fix file-cache records/blobs not replicating cross-node (ms-dev → ms-dev-2): on ms-dev-2
`caco file sync --mode pull` got 0 pulled and `caco file get` returned not-found, because the
producing node's pushed cacophony-state commit/blobs never reached the shared GitHub remote.

## Bead(s)
- bd-be6d54 (P3 bug, daemon/file-cache/replication). Confirmed root cause + contained fix.

## Root cause (confirmed)
`ensure_state_sync_repo` (caco-cli file_cmd.rs) set the file-cache state-sync repo's remote to
`current_git_remote_url("origin")` — the AGENT CHECKOUT's `origin`, which on managed nodes is a
NODE-LOCAL daemon/canonical mirror (verified: `git remote get-url origin` =
`$CACOPHONY_DIR/daemon/checkouts/cacophony`, not GitHub). So `git push origin
HEAD:refs/heads/cacophony-state` pushed blobs to the node-local mirror; they never reached the
shared GitHub cacophony-state, so other nodes pulling from THEIR mirror never saw them. The true
shared remote IS available to managed agents as `CACO_PROJECT_REMOTE_URL`
(= ssh://git@github.com/harryaskham/cacophony.git), but the sync ignored it.

## After state
- Pure `file_sync_shared_remote_override(remote, project, current_project_env, project_remote_env)`:
  for `remote == "origin"` syncing the CURRENT project (`project == CACO_PROJECT`) with a non-empty
  `CACO_PROJECT_REMOTE_URL`, returns that shared remote; otherwise None (non-origin, cross-project,
  or env-unset → fall back to the checkout origin, preserving non-managed behavior).
- `resolve_file_sync_remote_url(remote, project)` wraps it (reads the two env vars) and falls back to
  `current_git_remote_url`. `ensure_state_sync_repo` now uses it instead of `current_git_remote_url`.
- So a managed agent's `caco file sync` push/pull goes to the shared GitHub cacophony-state →
  records/blobs replicate cross-node. Gated + low-risk: only changes behavior for managed agents on
  the current project (where the broken status quo was silent non-replication); worst case is a
  retryable push error instead of silent loss.
- 1 unit test for the pure override decision.

## Diff summary
- crates/caco-cli/src/file_cmd.rs: file_sync_shared_remote_override (pure) + resolve_file_sync_remote_url
  + use it in ensure_state_sync_repo; 1 unit test.
(Final landed squash SHA: see the reintegration receipt.)

## Operator takeaway
Managed-agent file-cache sync now pushes/pulls the shared GitHub cacophony-state remote instead of the
node-local daemon mirror, so file-cache records/blobs produced on one node (e.g. ms-dev) replicate to
others (e.g. ms-dev-2) — removing the land-PNGs-to-main workaround for shared images. Cross-node
confirmation (produce on ms-dev → `caco file sync --mode pull` + `caco file get` on ms-dev-2) is the
recommended post-land verification; the root cause + pure decision are unit-covered.
