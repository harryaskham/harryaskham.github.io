# Session summary — bd-dcd2b6: per-agent checkout isolation guard for git mutations

## Goal

Harden against the cross-agent isolation breach where a collab/Tendril worker
ran `git commit` inside a FOREIGN agent's checkout (the update-helper release
agent), leaving a stale `.git/index.lock` and interrupting that agent's release.
The daemon already scopes the git operations it launches to the owning agent's
checkout, so the defensible in-repo fix (named in the bead's own triage) is a
worker-side guard that refuses git mutations targeting any directory other than
the agent's own checkout.

## Bead(s)

- `bd-dcd2b6` — Collab-mode worker ran git commit inside a FOREIGN agent's
  checkout (update-helper), leaving stale index.lock. P2 bug.

## Before state

- Failing tests: none; this is a runtime isolation hardening, not a test fix.
- No checked-in worker-side guard existed: managed Pi workers' `bash` tool ran
  any git command in whatever cwd the shell resolved, so a `cd <foreign-checkout>
  && git commit` (or a mis-resolved process cwd) could mutate another agent's
  working directory. The bead triage (aur-3) confirmed the daemon Rust is not
  mis-targeting and that cross-agent foreign-lock cleanup would itself be an
  isolation breach, leaving a worker-harness `pwd == $CACO_DEV_DIR` assertion as
  the correct fix.

## After state

- Failing tests: none. New node test `caco-checkout-guard.test.mjs` passes
  (run via plain `node`, matching the existing image-guard test convention) and
  covers the exact incident shape plus the full allow/deny matrix. `image-guard`
  test still passes (shared dep sanity). `caco config validate` clean; docs
  checks (`just docs-profiles-check-source-light`, `docs/validate-pages.sh` —
  3777 passed/0 failed) clean.
- New repo-owned Pi overlay `.cacophony/pi/checkout-guard/` wraps Pi's `bash`
  tool: it parses git MUTATION verbs (commit/add/reset/checkout/push/clean/
  config/...) and refuses when the effective target is outside `CACO_DEV_DIR`
  (leading `cd <foreign>`, process cwd already outside, or `git -C <foreign>`).
  Read-only git and non-git commands pass through; fails open with no
  `CACO_DEV_DIR`; `CACO_PI_CHECKOUT_GUARD_DISABLED=1` is the operator escape hatch.
- New mixin `.cacophony/profiles/pi-checkout-guard.md` wired into
  `pi-common.yaml` (all managed Pi agents), the `profiles.yaml` registry, and the
  ephemeral worker stack in `project.yaml`.

## Diff summary

- Code/content commit: a951efc34eec3fa387b01b579d066bab319a7eee (final landed
  squash SHA will come from the reintegration receipt).
- Files touched: `.cacophony/pi/checkout-guard/extensions/caco-checkout-guard.mjs`,
  `.../caco-checkout-guard-utils.mjs`, `.../caco-checkout-guard.test.mjs`,
  `.cacophony/profiles/pi-checkout-guard.md`, `.cacophony/agents/pi-common.yaml`,
  `.cacophony/profiles.yaml`, `.cacophony/project.yaml`, `AGENTS.md`,
  `docs/profiles.html`.
- Tests: +1 node test file (10+ assertions); no Rust tests changed.
- Behavioural delta: managed Pi workers can no longer run a git mutation outside
  their own checkout via the `bash` tool; the breach is refused loudly with the
  own/target dir and reason. Read-only inspection of any path is unaffected.

## Operator-takeaway

The fix is intentionally defensive-in-depth, not a root-cause rewrite: the
original cwd-mis-resolution lived in the worker launch/runtime layer (largely
out of this repo), so this adds a checked-in, tested worker-side gate that makes
the specific failure (git mutation in a foreign checkout) impossible to execute
through Pi's bash tool. Reflection draft bd-495466 was filed: the repo's Pi
overlay `*.test.mjs` files (including this one) have no automated CI runner and
only pass when run by hand — worth a small node-only test lane so guard
regressions are caught.
