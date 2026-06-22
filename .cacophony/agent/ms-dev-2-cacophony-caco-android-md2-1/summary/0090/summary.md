# bd-92b904 — Android QuickFile dialog image upload (slice A)

## Goal
Add image-upload to the Android companion quick-file bead dialog so an operator
can attach an image that the daemon's vision model describes + expands into
beads. (bd-92b904 also asks for a 'one bead' mode — that is slice B, a follow-up
land on the same claim; this land delivers the image-upload acceptance criterion.)

## Bead(s)
- bd-92b904 (Android quick file missing 'one bead' mode and image upload in
  dialog) — slice A: image upload. Claim held; NOT closed (one-bead mode pending
  as slice B).

## Before / After
- Before: QuickFileBeadDialog only had "Expand with AI" (text → POST
  /beads/expand → multiple bead drafts). No way to attach an image.
- After: an "Attach image" affordance (GetContent picker, reusing the proven
  Files image-share helpers) lets the operator attach an image; on expand the
  bytes are base64-encoded off the main thread and passed to
  expandBeads(imageBase64, imageMediaType). The daemon's vision model describes
  the image + expands that description into beads (bd-f04b54 image→beads
  backend; the daemon expand-forward preserves image_base64/image_media_type,
  verified). The attached image clears after a successful expand. An AssistChip
  shows the attached image name + a remove (x).

## Diff
- companion/android/app/src/main/java/com/cacophony/companion/connection/ConnectionManager.kt:
  expandBeads gains backward-compatible imageBase64/imageMediaType params →
  image_base64/image_media_type body keys (existing text-only call sites
  unchanged).
- companion/android/app/src/main/java/com/cacophony/companion/ui/quickfile/QuickFileBeadDialog.kt:
  GetContent image picker (reusing loadFilesImageShareDraft /
  readFilesImageShareBytes), base64 encode, attach-image UI (TextButton +
  AssistChip with remove), expand-call image wiring, success clear, 3 a11y
  content-description helpers.
- companion/android/app/src/test/java/com/cacophony/companion/QuickFileImageUploadSourceTest.kt:
  NEW source-pin + callable-a11y test (read() covers all 3 checkout roots).
- companion/android/app/src/test/java/com/cacophony/companion/QuickFileBeadDialogTest.kt:
  bd-e5b2e1 assertion updated to align with the new multi-line expandBeads call
  (intent preserved: snapshot project/text before the coroutine launch).

## Embedded artefacts
- Validated: queued tj-eebc1000 (nix develop .#android-validation --command
  gradle :app:testDebugUnitTest --tests QuickFileImageUploadSourceTest --tests
  QuickFileBeadDialogTest) — 18 tests, all passed.

## Operator-takeaway
The Android quick-file dialog now supports attaching an image for image→beads
expansion, consistent with the daemon's vision backend. 'One bead' mode (the
dialog's other AC) lands next as slice B; bd-92b904 stays open until both done.

## Diff summary
Landed commit: see the reintegration receipt (Android-only slice A:
ConnectionManager + QuickFileBeadDialog + the two test files).
