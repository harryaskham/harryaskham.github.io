# bd-92b904 — Android QuickFile dialog one-bead mode (slice B) — COMPLETES bead

## Goal
Complete bd-92b904: add a 'one bead' mode to the Android quick-file dialog
(slice A image upload landed as 7122594e9). One-bead mode creates a single bead
directly from the input instead of AI-expanding into multiple drafts.

## Bead(s)
- bd-92b904 (Android quick file missing 'one bead' mode and image upload in
  dialog) — slice B: one-bead mode. Both acceptance criteria now done →
  CLOSING bd-92b904.

## Before / After
- Before: the dialog only AI-expanded text into multiple bead drafts (POST
  /beads/expand).
- After: a "One bead" toggle (FilterChip) makes the action button create a
  SINGLE bead directly via createBead — title = first non-blank line (truncated
  to 120 + ellipsis), description = full text (no data loss) — instead of
  expanding. The button label switches to "Create bead". A confirmation
  ("Created 1 bead: <title>") shows on success.

## Diff
- companion/android/app/src/main/java/com/cacophony/companion/ui/quickfile/QuickFileBeadDialog.kt:
  oneBeadMode state, "One bead" toggle FilterChip, the createBead branch (via
  return@safeLaunch), conditional button label, + 3 helpers
  (quickFileOneBeadFields, quickFileOneBeadCreatedCopy,
  quickFileOneBeadToggleContentDescription).
- companion/android/app/src/test/java/com/cacophony/companion/QuickFileOneBeadModeSourceTest.kt:
  NEW callable-helper + source-pin test.

## Embedded artefacts
- Validated: queued tj-3567faf3 (gradle :app:testDebugUnitTest --tests
  QuickFileOneBeadModeSourceTest --tests QuickFileImageUploadSourceTest --tests
  QuickFileBeadDialogTest) — all passed.

## Operator-takeaway
The Android quick-file dialog now supports both acceptance criteria of
bd-92b904: image upload (slice A) + one-bead mode (slice B). Closing bd-92b904.

## Diff summary
Landed commit: see the reintegration receipt (Android-only slice B).
