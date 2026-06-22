# bd-a30f5e — Android image share-intent → image→beads — COMPLETES bead

## Goal
Complete bd-a30f5e (Android Quickfile image upload + share-intent, image→beads).
The image-upload acceptance criterion landed earlier as bd-92b904 slice A
(7122594e9); this land adds the share-intent: sharing an image into the app
shortcuts into the image→beads expand flow.

## Bead(s)
- bd-a30f5e (Android Quickfile image upload + share intent (image→beads)) —
  share-intent slice. Both ACs now done (image upload via bd-92b904, share-intent
  here) → CLOSING.

## Before / After
- Before: sharing an image (ACTION_SEND image/*) only uploaded it to file-cache
  (bd-b50965) and associated the file id with any beads created.
- After: the shared image is ALSO pre-attached into the QuickFileBeadDialog
  image→beads flow (initialImageDraft), so the user can tap Expand with AI to
  have the daemon vision model describe the image and expand it into beads. The
  manifest already accepts image/*. QuickFileImageShareMetadata →
  FilesImageShareDraft is a 1:1 field map.

## Diff
- companion/android/.../ui/quickfile/QuickFileBeadDialog.kt: new initialImageDraft
  param → imageDraft initialized from it (pre-attach).
- companion/android/.../widgets/QuickFileWidgetActivity.kt: converts the first
  shared image (imageShareMetadata.firstOrNull isImageShare) → FilesImageShareDraft,
  passes as initialImageDraft + imports it.
- companion/android/.../ui/files/FilesScreen.kt: FilesImageShareDraft made public
  (it is now a public dialog param type; the load/read helpers stay internal).
- companion/android/.../test/.../QuickFileImageShareIntentSourceTest.kt: NEW
  source-pin test.

## Embedded artefacts
- Validated: queued tj-fa30494b (gradle :app:testDebugUnitTest --tests
  QuickFileImageShareIntentSourceTest + the QuickFile + FilesRecencySort suites)
  — all passed. (A first attempt tj-99cf82e1 caught a real public-exposes-internal
  visibility error, fixed by widening FilesImageShareDraft to public.)

## Operator-takeaway
Android: sharing an image into the app now shortcuts into the image→beads
quickfile flow (in addition to the existing file-cache upload). bd-a30f5e
complete (image upload via bd-92b904 + share-intent here). Closing.

## Diff summary
Landed commit: see the reintegration receipt.
