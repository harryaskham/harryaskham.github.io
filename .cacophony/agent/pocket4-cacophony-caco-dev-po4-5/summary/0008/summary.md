# Session summary — bd-257ad6 superseded by concurrent fix on main

## Goal

Close bd-257ad6 (P3, profile-audit): every persistent agent's reified
system prompt carried two contradictory rules — one forbidding manual
`caco bd close` (correct only for one-shot bead-workers), one
requiring it after every reintegration (msd-2 close-discipline,
correct for persistent / endless agents).

## Bead(s)

- `bd-257ad6` — [profile-audit][caco-dev] 'DO NOT call caco bd close'
  (line 52) contradicts msd-2 close-discipline (line 1447). Filed
  earlier this session by po4-5 during the operator-requested profile
  audit pass.

## Before state

- `crates/caco-profile/src/common_instructions.txt:52` carried an
  absolute prohibition: `**DO NOT** call \`caco bd close\` yourself`.
- `common_instructions()` is composed into the prompt of EVERY agent
  via `role_instructions(is_persistent, is_worker)`.
- Persistent profiles layer on the msd-2 close-discipline mixin which
  says the OPPOSITE.

## After state

- Implementation landed on main BEFORE this agent could reintegrate.
  Concurrent agent po4-1 closed bd-b8cb1c at 20:58:19 with a commit
  (0a55502b4) that rewrote the same paragraph in
  `common_instructions.txt` to a role-aware version, incidentally
  fixing bd-257ad6 too.
- po4-5 had a local commit (7922a510b) with substantively the same
  fix plus two regression tests; on rebase the two attempts conflicted
  and po4-5 dropped the redundant code commit. Tests not contributed
  this round.
- bd-257ad6 closed via admin-override referencing 0a55502b4 as the
  landed work.

## Diff summary

- No code commit from this agent this session — landed work is at
  0a55502b4 (po4-1 / bd-b8cb1c).
- This summary is the only artefact.

## Operator-takeaway

Two pocket4 dev agents independently spotted the same line-52
contradiction in the same audit-pass window and shipped near-identical
fixes; only one could land. The race resolved cleanly on rebase
(no lost work, no duplicate paragraphs in main) but it does indicate
that the six profile-audit beads filed by po4-5 should be reviewed
for already-fixed status before further claims — at minimum
bd-b8cb1c's fix has knock-on coverage of bd-257ad6 territory. No
regression tests were added this round; if operators want lock-in
against future regressions, that's a small follow-up.
