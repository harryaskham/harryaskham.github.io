# Session summary 0049 — bd-34bca7: dev.md bead-closure discipline

## Goal

Codify the bead-closure protocol in the canonical dev profile
so workers (including future-me) don't leak claims like the
five msm-5 caught tonight.

## Bead(s)

- `bd-34bca7` — profile-doc only.

## Before state

- `dev.md` Completion section said "close it" but didn't
  cover (a) what to do when the close-validator rejects, (b)
  the unclaim-vs-close decision, or (c) session-start audit.
- msm-5 had 5 stale `in_progress` beads from prior sessions;
  one (bd-b69cf3) had been concurrently completed by msd-4.
  Naive `unclaim` of all five would have lost the audit trail
  on bd-b69cf3.

## After state

- New "Bead-closure discipline (msd-2 protocol)" subsection
  under `## Completion` in `.cacophony/profiles/dev.md`:
  - Every reintegrated bead MUST be closed.
  - `--validate-on-main false` for landed beads (squash-footer
    miss; bd-845653 root cause).
  - `--admin-override --reason "..."` for beads with no commits.
  - Pre-unclaim check: `caco bd show` assignee +
    `git log origin/main --grep=<id>`. Close instead of
    unclaim if another worker took it on or landed it.
  - Session-start audit:
    `caco bd list --assignee cacophony:$CACO_AGENT_ID --status
    in_progress --limit 20`.

## Diff summary

- Commit: pending reintegrate.
- Files (1): `.cacophony/profiles/dev.md` (+31 lines).

## Operator-takeaway

Workers spawning under `dev` profile from now on get the audit
+ closure rules in their startup context, so prior-session
orphans should self-heal at session start instead of
accumulating. Operator nudge "follow dev-2's bead cleanup
protocol" is now the documented default.
