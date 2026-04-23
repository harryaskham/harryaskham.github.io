# Session summary — bd-cb44d9 root cause + bd-a2ace0 re-land

## Goal

Root-cause **bd-cb44d9** (P1 bug — bd-a2ace0 axe-core test pass closed in
beads-db but commit 572feaaa missing from origin/main). The bead listed
three hypotheses (merge-queue rollback, destructive reconcile,
force-push race); my job was to identify the actual cause, file the
defense-in-depth bead, and re-land the lost work in the same session.

## Bead(s)

- **bd-cb44d9** (P1 bug, root-cause + recovery — owned)
- **bd-a2ace0** (P3 re-land, axe-core a11y test pass — recovered as part
  of the same session; was already reopened by caco-ctrl)
- bd-fe65b4 (P1 bug, defense — filed: atomic close gate)
- bd-398b0e (P2 task, audit — filed: sweep historical close events for
  silent-loss divergences)

## Before state

- 572feaaa not reachable from origin/main; bd-a2ace0 marked closed in
  beads-db; vendored axe.min.js + harness JS + 3 axe Rust tests all
  missing from main.
- No postmortem doc, no defense-in-depth bead, no audit task.
- Three loose hypotheses on the original bead, none correct in
  isolation.

## After state

- **Root cause identified — hypothesis #4 (not in the original list).**
  Daemon reflog evidence in
  `/Users/harryaskham/.cacophony/daemon/checkouts/cacophony` shows the
  pattern `commit (572feaaa) → reset --hard origin/main → reset` fired
  twice for msm-5 in 1 minute. The daemon's reintegration flow commits
  locally, marks the bead closed in beads-db, **then** pushes; on
  push-failure (non-FF — companion landed in the same window) it does
  `git reset --hard origin/main`, silently discarding the local commit
  while beads-db has already persisted the close.
- Postmortem written:
  `docs/postmortems/bd-cb44d9-reintegration-silent-loss-2026-04-23.md`
  with reflog evidence, root cause, recommended fixes (atomic close
  gate, replace reset with rebase, audit-log resets, sweep historical
  closes), and explicit cross-reference to bd-cf99b7 family.
- Defense bead **bd-fe65b4** P1 filed: atomic close gate (close after
  push succeeds, replace reset with rebase, audit log).
- Audit bead **bd-398b0e** P2 filed: sweep last 14 days of close
  events for silent-loss divergences.
- bd-a2ace0 re-landed by cherry-picking the original commit `a2c22299`
  from `origin/agent/...msm-5` (still alive on the agent branch).
  Conflict in `crates/caco-web/src/tests.rs` resolved by keeping both
  the existing tests (already in main from later reintegrates) and
  the three new axe tests.
- 120/120 caco-web lib tests green.

## Diff summary

- Commits:
  - `09416a1e` — bd-a2ace0: in-repo axe-core a11y test pass over
    rendered pane HTML (cherry-picked verbatim from a2c22299).
  - `0ad4f7e0` — bd-cb44d9: postmortem + re-land bd-a2ace0.
- Files touched:
  - `docs/postmortems/bd-cb44d9-reintegration-silent-loss-2026-04-23.md`
    (+73, new)
  - `crates/caco-web/src/tests.rs` (+170, axe tests)
  - `crates/caco-web/static/vendor/axe/{README.md,axe.min.js}` (new)
  - `crates/caco-web/tests/{.gitignore,axe_harness.js,axe_self_test.js}`
    (new)
- Tests: +3 (axe); 120/120 lib green.
- Behavioural delta: axe-core a11y test pass restored; new postmortem
  doc; two new beads filed with full context.

## Operator-takeaway

The original bd-cb44d9 bead listed three hypotheses and none of them
were correct on their own — the actual cause was hypothesis #4: the
daemon's *recovery* path (reset --hard) was destructive against
unpushed local commits, and the bead-close call was sequenced before
the push, so a push-failure left state divergent. The fix
(bd-fe65b4) is small and mechanical: move the close call after push
succeeds, replace reset with rebase. The audit (bd-398b0e) protects
against any other silent-loss instances in the last 14 days that
nobody has noticed yet.

The postmortem also nails down that the "5 ⚠ DESTRUCTIVE bd
reconciles" the controller alerted on (later retracted as
bd-96dcf5 false-alarm) are an UNRELATED bead-database
shrink-event class, not the same bug as this commit-loss class —
so investigators don't conflate them next time.
