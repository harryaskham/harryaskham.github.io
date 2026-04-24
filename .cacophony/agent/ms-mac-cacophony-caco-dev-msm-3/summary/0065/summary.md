# Session summary 0065 — bd-31f836 pre-push hook

## Goal

Stop worker processes from force-pushing to refs/heads/main /
refs/heads/beads / another agent's branch while leaving own-branch
push (the normal rebase-and-push workflow) alone.

## Bead(s)

- bd-31f836 — block worker push to default / shared branches

## Before state

- Workers could 'git push --force' to any ref including refs/heads/main
  and refs/heads/beads, sidestepping the reintegrate audit gate.

## After state

- Client-side pre-push hook (committed at .cacophony/git-hooks/pre-push)
  rejects pushes to protected refs unless CACO_LIFECYCLE_PUSH=1.
- Hook auto-installed into every new managed checkout via
  agent::push_discipline::install_pre_push_hook from spawn::clone_checkout.
- All daemon-internal push call sites in reintegration.rs auto-set
  the lifecycle-bypass env var.
- Own-branch push (refs/heads/agent/<self>) allowed unconditionally.

## Diff summary

- Commit: 3c0f6876bab6
- Files: .cacophony/git-hooks/pre-push (new); crates/caco-daemon/src/agent/
  {push_discipline.rs (new), mod.rs, spawn.rs}; crates/caco-daemon/src/
  reintegration.rs (run_git_ssh + run_git_with_identity); docs/profiles.html
  (drive-by main red).
- Tests: +3 (embedded-content guard + e2e install + idempotency).

## Operator-takeaway

You can keep using 'git push --force-with-lease' on your own branch
for the rebase + reintegrate workflow. Pushes to main/beads from a
worker checkout will now error with a message naming
'caco agent reintegrate' / 'caco agent complete' as the remediation.
