# Session summary — bd-1c1d0f Issue 13: bd reconcile-log --project '' upfront guard

## Goal

Pin Issue 13 of the bd-1c1d0f bd-graph+reconcile-log+stats sweep:
`caco bd reconcile-log --project ''` echoed an empty quoted string
into the path-context error (`project "" has no beads .git
checkout at /home/harry/.cacophony/beads/`) — 15th empty-string-
bypass surface in the cluster-wide bd-29c7e3 family. Add upfront
guard.

## Bead(s)

- `bd-1c1d0f` — caco bd graph + reconcile-log + stats sweep (P4
  bug, multi-issue). Pins Issue 13. Issues 1-11 are POSITIVES (a
  RICH gold-standard cluster: NEW '(got X)' parenthesized-actual
  phrasing 7th in cohort; GOLD-STANDARD numeric-edge-case error
  WITH USAGE GUIDANCE on bd graph --depth 0 — strong promote
  candidates). Issue 12 (--depth -1 / --limit -1, 12th+13th
  parser-ambiguity surfaces) is covered by bd-02c404 cross-cutting
  parser meta-bead. Issue 14 (operational bd-cf99b7 destructive
  delta=-1 markers every 30s) belongs to caco-ctrl@helsinki sole-
  owner per operator constraint.

## Before state

```
$ caco bd reconcile-log --project ''
error: project "" has no beads .git checkout at /home/harry/.cacophony/beads/; reconcile-log only works on projects with a local beads-branch checkout
```

The path-context error was the right shape for typo'd project
names but was the wrong error for empty-string input. Operator
sees the empty quoted name leaked into a filesystem path and
must mentally back-translate to figure out they passed an empty
flag.

## After state

```
$ caco bd reconcile-log --project ''
error: --project value cannot be empty for bd reconcile-log
```

Matches the bd-9d3623 msg-snapshot gold-standard empty-string
template. The downstream path-context error remains the right
shape for genuine typos (`bd reconcile-log --project bogus`).

## Diff summary

- 1 file changed, +12 / -0 (`crates/caco-cli/src/lib.rs`):
  - `dispatch_bd_reconcile_log` checks `--project` for whitespace-
    only / empty before flowing into `resolve_project_from_flags_or_env`.

## Validation

- `cargo check -p caco-cli`: clean.

## Operator-takeaway

`caco bd reconcile-log --project ''` now produces the canonical
empty-string error, joining the bd-9d3623 / bd-d761db / bd-754fde
/ bd-754fde / bd-87425e / bd-89df3d / bd-754fde / bd-5f81fe family
of upfront `cannot be empty for X` guards. 15 surfaces patched
individually now; the cross-cutting `validate_non_empty_id` helper
(bd-29c7e3) is well-justified.

Push-discipline (post-clarification): own-branch push allowed;
default-branch force-push banned; only reintegrate / complete
land work on main. This session continues to use only local refs
+ daemon-mediated reintegrate.
