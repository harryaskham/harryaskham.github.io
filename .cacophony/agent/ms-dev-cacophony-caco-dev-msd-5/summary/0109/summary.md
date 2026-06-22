# Session summary — PR head pushes to the configured forge, not the daemon mirror (bd-6e9810)

## Goal

`caco agent reintegrate --mode pr_*` / `--create-pr` against a project whose push
`remote` is the daemon-local mirror (the agent-origin=daemon-checkout topology)
force-pushed the PR head to that LOCAL MIRROR, so `gh pr create` could never find
the head branch on GitHub — PR-backed reintegration was broken for the standard
managed-agent topology. This routes the PR head to the project's configured forge
so the PR opens.

## Bead(s)

- `bd-6e9810` — PR-mode reintegration `--create-pr` pushes to agent-origin (daemon
  checkout) not GitHub (P2, fleet-default blocker).

## Before state

- Failing tests: none.
- `reintegrate_direct_branch` pushed the PR head to `remote` (= the daemon-local
  mirror), verified via the `{remote}/{target}` tracking ref, then `gh pr create`
  failed because the head was never on the forge.

## After state

- Failing tests: none.
- When `pr_base.is_some()` AND `remote` resolves to the daemon-local mirror, the
  PR head (and recorded cacophony-state) route to the configured forge URL —
  resolved via the mirror's own `origin` (which IS `projects[].remote`, since the
  mirror is a clone of it). Push is by-URL with an explicit `--force-with-lease`
  (no local tracking ref exists for a push-by-URL; expected-old via `ls-remote`),
  verified via `ls-remote` on the forge. `open_or_update_pr` is UNCHANGED (project
  gh wrapper + GH_REPO/CACO_GH_AUTH_USER, never agent-direct gh), now finding the
  head on GitHub. Non-PR flows + already-forge remotes keep the push-by-name path.
- An `elog!` diagnostic (EPIPE-safe) records the mirror->forge routing for operator
  observability.

## Diff summary

- File: `crates/caco-daemon/src/reintegration.rs` only.
- Added: `pr_head_forge_url_for_mirror_remote` (mirror-chain forge resolution),
  `ls_remote_branch_sha` + `parse_ls_remote_branch_sha` (pure), `force_with_lease_arg`
  (pure), `push_pr_head_to_forge_url`; the push/verify/state forge-URL branching;
  the elog! diagnostic.
- Tests: +2 pure (`force_with_lease_arg_bd_6e9810`, `parse_ls_remote_branch_sha_bd_6e9810`).
- Validation: cargo check --workspace --tests GREEN (rc=0); real apple-utils PR
  round-trip PROVEN (mirror origin -> resolved forge ssh://...apple-utils.git ->
  push-by-URL -> head 66323f4 on github.com/harryaskham/apple-utils ahead of main
  -> PR https://github.com/harryaskham/apple-utils/pull/2 opened; throwaway cleaned up).
- Final landed squash SHA from the reintegration receipt.

## Operator-takeaway

PR-backed reintegration now works on the standard managed-agent topology (agent
origin = daemon mirror): the PR head reaches the configured forge so gh pr create
opens the PR. Reviewed by md2-1 (Harry's invariant honored: routes through
projects[].remote, never agent-direct gh). Separate finding worth a follow-up:
the apple-utils project config does not export GH_REPO, so its PR flow needs that
config gap closed (cacophony exports it, so bd-6e9810 works end-to-end there).
