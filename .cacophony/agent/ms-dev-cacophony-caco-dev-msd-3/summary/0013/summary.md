# Session summary — bd-398b0e: 14-day silent-loss sweep

## Goal

Per bd-cb44d9 postmortem follow-up: sweep all bead closures in the
last 14 days and verify each landed on origin/main, reopening any
that didn't (silent-loss candidates).

## Bead(s)

- `bd-398b0e` — Sweep last 14 days of bead close events for
  silent-loss divergences (bd-cb44d9 follow-up)

## Before state

- bd-cb44d9 postmortem identified one severe silent-loss
  incident (premature bead-close + reset-induced commit drop)
  with reflog evidence on bd-a2ace0 (572feaaa, 467f3eb7).
- No prior systematic sweep of the 14-day window had been done;
  unknown how many other beads might be in the same shape.
- bd-fe65b4 slice 1 (force_reset audit logging) had just landed
  earlier this session, providing detection-going-forward but
  no retrospective coverage.

## After state

- 14-day sweep complete: ZERO silent-loss incidents in window.
- Two real-agent beads with no commit-trace (bd-10e37c, bd-24ada6)
  manually verified as fixed via functional checks.
- Sweep report committed to docs/sweeps/ for future reference.
- Two hygiene follow-ups identified (bead-id footer enforcement;
  landed_commit population) but not actioned in this session.

## Method

For each of 652 closed beads in window:
1. `git log origin/main --grep "$BID"` (commit-message check)
2. `git log origin/main -S "$BID"` (pickaxe — content check)
3. Bucket and triage misses by assignee shape.

## Result

ZERO silent-loss incidents.

| bucket | count |
|---|---:|
| Found via --grep | 451 |
| Missing-grep, agent-assigned | 48 |
| Missing-grep, no agent (admin closure) | 153 |
| Missing BOTH grep AND pickaxe | 8 |

The 8 truly-invisible beads broke down as:
- 6 ephemeral test-agent assignees (not real prod work)
- bd-10e37c: cargo test passes on main → fix landed
- bd-24ada6: required field present in all named files → fix landed

Both real-agent beads were verified fixed via direct functional
checks, not commit-trace; the bead-id just wasn't recorded in
the squash subject or in code.

## Diff summary

- `docs/sweeps/bd-398b0e-silent-loss-sweep-2026-04-23.md` (+76):
  full report with method, buckets, per-bead verdicts, and two
  follow-up hygiene observations.

## Embedded artefacts

- The sweep report is committed under docs/sweeps/.

## Operator-takeaway

The destructive-reconcile family (bd-cb44d9, bd-cf99b7) is
currently quiescent — no closures-without-landings in the last
14 days. Combined with bd-fe65b4 slice 1 (force_reset audit
logging, landed earlier this session), the next incident will
leave a forensic trail instead of disappearing silently.

Two hygiene gaps worth filing as follow-ups (not done in this
session to keep scope tight):
1. Squash-commit template doesn't enforce a bead-id footer —
   2/48 audited beads landed without their bead-id appearing
   anywhere on main. Easy to fix in the reintegrate-template
   path; would make future sweeps ~zero-noise.
2. `landed_commit` on bead records is never populated by the
   reintegrate flow (verified by checking my own bd-5bae2e and
   bd-fe65b4 closures from earlier today). Filling it in would
   make sweeps O(n) lookups instead of O(n × git-log).
