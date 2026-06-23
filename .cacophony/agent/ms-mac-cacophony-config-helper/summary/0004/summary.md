# Session summary — bind picasso to the msft.ghe.com git identity

## Goal

Fix picasso's daemon-managed git authentication. picasso's remote is
ssh://msft@msft.ghe.com/... but it was bound to identity harryaskham_microsoft,
whose hostname was null — so with the username `harryaskham` colliding across
github.com (personal) and msft.ghe.com (corp), the daemon could pick the wrong
SSH key by username, forcing the operator to manually override GIT_SSH_COMMAND
on every node to fetch/reset picasso's daemon checkout.

## Bead(s)

- (operator request, Harry 2026-06-23) — no implementation bead; config-only.
- Relates to bd-f5bc09 (identity hostname host-matching).

## Before state

- Failing tests: none (config-only)
- picasso.identity = harryaskham_microsoft (a github.com identity, ~/.ssh/caco-work,
  hostname null). Operator had to run GIT_SSH_COMMAND="ssh -i ~/.ssh/corp-github-key"
  git fetch/reset by hand on each node's daemon checkout.
- Harry landed a new identity `harryaskham_ghe` (user harryaskham@microsoft.com,
  key ~/.ssh/caco-work, hostname: msft.ghe.com). SSH probe confirmed caco-work
  authenticates to msft.ghe.com as harryaskham.
- `caco config validate`: ok

## After state

- Failing tests: none
- picasso.identity = harryaskham_ghe (hostname msft.ghe.com → host-based SSH key
  selection, sidestepping the colliding `harryaskham` username). mono (a
  github.com project) correctly stays on harryaskham_microsoft.
- `caco config validate`: ok

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt
- Files: `.cacophony/projects.yaml` (picasso.identity: harryaskham_microsoft → harryaskham_ghe)
- Tests: +0 / -0
- Behavioural delta: daemon-managed git for picasso now selects the msft.ghe.com
  identity/key by host; no more manual GIT_SSH_COMMAND override needed on future
  daemon git operations once each node's daemon reloads config.

## Operator-takeaway

picasso now binds to a host-scoped msft.ghe.com identity, so the daemon picks the
right SSH key by host rather than by the ambiguous `harryaskham` username. Applies
to FUTURE daemon git ops after each node's checkout sync / daemon config reload —
not retroactive to past operations, and a daemon restart per node guarantees pickup.
