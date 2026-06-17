# Session summary — bd-6d5d67: Android "What's New" / Changelog screen

## Goal
Add a dedicated "What's New" / Changelog screen to the native Android companion that
surfaces landed work grouped by RELEASE VERSION — the Android slice of the bd-ac3db4
cross-surface "what's new" umbrella (operator-requested, blanket-approved).

## Bead(s)
- bd-6d5d67 (filed + claimed as the clean self-contained Android child of bd-ac3db4).

## Before/After state
- Before: the app had ui/releases/ReleasesScreen (release JOBS) + ui/timeline/TimelineScreen
  (activity), but NO "what just shipped" view — the in-app "changelog" nav route just ALIASED
  to the Releases screen (MainActivity ~L630). No parity with the other clients' what's-new.
- After: a real ChangelogScreen surfaces GET /api/v1/projects/{project}/changelog (beads grouped
  by release version) as version-headed cards with color-coded type chips + bead ids. The
  "changelog"/"whats-new" nav route now opens it; a "What's New" More-menu entry was added.

## Diff summary
- Code/content commits: pending reintegration receipt SHA.
- Files: connection/ChangelogModels.kt (ChangelogRelease/ChangelogBead + fromJson); ConnectionManager.getChangelog;
  ui/changelog/ChangelogScreen.kt (Scaffold + version sections + type chips, ChangelogContent extracted for the harness);
  MainActivity.kt (route split, title, renderer, More-menu entry); src/debug ChangelogDebugActivity + manifest (QA harness, not shipped);
  test/ChangelogScreenTest.kt (6 tests). No daemon work — endpoint already exists (bd-995ea6).

## Embedded artefacts
- 6/6 ChangelogScreenTest (model parse + defaults + project resolution + type colors + load-failure copy).
- assembleDebug BUILD SUCCESSFUL (full app packages with the new screen + nav).
- RENDER-VALIDATED on emulator-5554 via the debug harness (sample data): "What's New" header +
  "2 released versions • cacophony", version-headed cards (v1.2.515 green/bold + date + item count),
  color-coded type chips (bug=red, feature=green, task=blue), monospace bead ids, wrapping titles.
  Screenshot in file-cache: bd-6d5d67-changelog-whatsnew-render.png.

## Operator-takeaway
The Android app now has a dedicated "What's New" surface (More -> What's New) showing what shipped
per release version, at parity with the other clients' intent. Self-contained Android slice of the
bd-ac3db4 umbrella — no daemon change (the changelog endpoint already existed). I flagged to the
router that bd-de8c7a is a true duplicate of bd-ac3db4, and that the broader umbrella still needs
the TUI/web/iOS slices (and a decision on whether they derive client-side or need new endpoints).
