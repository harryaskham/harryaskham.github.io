# Session summary — bd-b0385c: Android bead view 5000-cap truncation indicator

## Goal
Operator-flagged (Harry, S24): the Android companion bead view shows only ~5k beads — the
bead-list endpoint caps at 5000 rows but cacophony has 10,824 beads, so listing all statuses
truncates (mostly old closed history hidden).

## Bead(s)
- bd-b0385c — Android bead view truncates at the 5000-row cap

## Before/After state
- Before: the default filter was already the small active "Ready/Open" slice (bd-78ded9), so
  active work (open+in_progress = 237) is visible. BUT for the All/Closed views (which exceed the
  cap) the existing rowsStillSyncing banner showed a MISLEADING perpetual "...detailed rows are
  still syncing... Pull to refresh" — it never catches up because it is CAPPED, not syncing.
- After: a new honest truncation indicator distinguishes "truncated at the 5000 history cap"
  (loaded set at the cap AND authoritative count higher) from transient syncing — "Showing the
  first 5000 of N <status> beads. This app loads up to 5000 beads, so older beads (mostly closed
  history) aren't loaded yet. Filter to active work, pick a project, or search to find a specific
  bead." (Frost2 + History icon, vs the yellow SyncProblem syncing banner).

## Diff summary
- Code/content commits: pending reintegration receipt SHA.
- ui/beads/BeadsListScreen.kt: BEAD_HISTORY_CAP = 5000 const; beadListTruncatedAtHistoryCap pure
  helper (searchQuery blank && loadedTotal >= cap && expectedCount > visibleCount); truncatedAtCap
  computation; the FilterSummaryBar branch now shows the cap banner first, else the syncing banner.
- test/BeadHistoryCapTruncationSourceTest.kt: 5 cases (at-cap-truncated, within-cap, all-visible-at-cap,
  while-searching, below-cap-is-syncing).

## Embedded artefacts
- BeadHistoryCapTruncationSourceTest 5/0; BeadsListDefaultSortSourceTest 3/0 +
  AggregateBeadCountsSinglePassSourceTest 2/0 (no regression); assembleDebug green.
- The banner reuses the already-render-validated FilterSummaryBar component (same as the syncing
  banner), so this is a low-risk props/condition change validated by the unit test + compile.

## Operator-takeaway
The Android bead view now honestly surfaces the 5000-row history cap: active work stays visible
(default Open filter), and the All/Closed views clearly say "showing first 5000 of N, older history
not loaded" instead of a misleading perpetual "syncing". REMAINING for full 10k reachability: a
daemon-side paginated/offset beads endpoint + an Android "load more" wired to it — a daemon/caco-web
slice (not the Android UI lane). bd-b0385c kept open for that follow-up; the Android UI part is done.
