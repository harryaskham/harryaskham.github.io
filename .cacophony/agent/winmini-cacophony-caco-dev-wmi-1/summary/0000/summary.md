# Session summary — bd-f4957c Issue 5: caco project status --project converge to security-WHY

## Goal

Pin Issue 5 of the bd-f4957c node+project sweep: `caco project
status --project bogus` returned a TRUNCATED `is not configured`
error — drifting from its sister `caco project show --project bogus`
(which uses the gold-standard security-WHY phrasing) WITHIN THE SAME
NAMESPACE. Converge to the canonical security-WHY phrasing now
shared by build list / changelog show / project show --project.

## Bead(s)

- `bd-f4957c` — caco node + project sweep (P3 bug, multi-issue).
  Pins Issue 5. Issues 1-2 are POSITIVES (BEST-IN-CACO error
  message + NEW multi-flag usage gold-standard). Issue 3 (project
  show --name vs --project drift) is a 3-line dispatcher fix but
  the operator likely wants `--name` removed entirely as a
  duplicate of `--project`; needs design call. Issue 4 is positive.
  Issue 6 (node show --json broken exit 2) is the same anti-
  pattern as bd-87425e — needs same wrapper-at-dispatch-boundary
  treatment as that surface; bigger fix. Issue 7 is the same
  warn-then-process family fixed for operator-actions in
  bd-851658 — node show --name + project show --name need ArgSpec
  hoists (defer for design call). Issue 8 is positive. Issue 9 is
  cohort observation (half-flat envelopes — same family as
  bd-b9eccd which wmi-2 just landed).

## Before state

```
$ caco project show --project bogus
error: project 'bogus' is not configured; bead operations must target a configured project to prevent routing to an ambient external board

$ caco project status --project bogus
error: project 'bogus' is not configured           # TRUNCATED — sister drift
```

## After state

```
$ caco project status --project bogus
error: project 'bogus' is not configured; bead operations must target a configured project to prevent routing to an ambient external board. Configured: a.skh.am, cacophony, collective, gfx-replacer, life, midi2hid, mono, picasso-health, tendril
```

Now matches `project show --project` + adds `Configured:` listing
(slight improvement over the existing security-WHY surfaces, which
do not list configured names — a pattern bd-7abbba Issue 4 also
flagged as a future improvement).

`--project` convention map after this fix:
- Security-WHY (gold-standard): build list, changelog show, project
  show --project, **project status** (4 surfaces).
- Truncated (degraded): project show --name (1, needs Issue 3 fix).
- Inline-allowed-values: fleet snapshot --projects (1).
- Silent-accept (worst): caco ls --project, caco ps --project
  (already fixed by wmi-2 in bd-b9eccd).

## Diff summary

- 1 file changed, +21 / -1 (`crates/caco-cli/src/lib.rs`):
  - `dispatch_project_status` empty-match branch upgraded to the
    security-WHY phrasing + Configured listing.

## Validation

- `cargo check -p caco-cli`: clean.

## Operator-takeaway

`caco project status` now matches its sister `caco project show`
on the unknown-project error path — same security-WHY phrasing,
plus the configured-project listing as a bonus typo affordance.

Push-discipline (post-clarification): own-branch push allowed;
default-branch force-push banned; only reintegrate / complete
land work on main. This session continues to use only local refs
+ daemon-mediated reintegrate.
