# Session summary — dev profile self-improvement (FF-recovery doc)

## Goal

Document the recurring "non-fast-forward" agent-branch push failure
that bit this session twice (bd-efe17d, bd-727210) so future workers
on the dev profile recover without ceremony and without reaching for
`git push --force*` (which the merge-queue mixin forbids).

## Bead(s)

- (no bead — direct profile self-improvement under the
  `self-improvement` mixin, prompted by the recurring footgun while
  reintegrating bd-727210)

## Before state

- `.cacophony/profiles/dev.md` documented merge-conflict recovery only.
- The non-fast-forward variant (where main is fine but the agent
  branch tip diverges from origin after a previous squash-merge) was
  not described, so each recurrence cost a fresh investigation.

## After state

- `.cacophony/profiles/dev.md` now has a "Recurring footgun" subsection
  that names the symptom, explains the cause, and documents the
  reset-and-cherry-pick recovery. Cross-references the merge-queue
  mixin's force-push prohibition.

## Diff summary

- Commits: `bd1668b7`
- Files touched: `.cacophony/profiles/dev.md` (+30 lines, additive)
- Tests: none (profile doc only).
- Behavioural delta: future agents on this profile have a documented,
  no-force-push recovery for the FF-rejected push case.

## Operator-takeaway

If `caco agent reintegrate` ever returns "non-fast-forward" without a
merge-conflict shape, that is the daemon-pushed-old-tip case, not a
real conflict. Reset the agent branch to its origin tip, cherry-pick
the new commits, retry the reintegrate. No force-push needed; main is
not at risk.
