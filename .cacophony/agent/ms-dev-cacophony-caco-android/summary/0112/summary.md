# Session summary — Android file-cache upload client helper

## Goal

Add a reusable Android client helper for the daemon's mobile-safe `POST /api/v1/file-cache` contract, without wiring any UI to upload images yet.

## Bead(s)

- `bd-047dc8` — Android file-cache upload client helper
- parent context: `bd-174386` — Mesh image sharing via existing caco file API

## Before state

- Failing tests: none known for this slice.
- Relevant metrics: Android image-sharing surfaces were placeholders only, and ConnectionManager could browse files but had no upload helper for `/api/v1/file-cache`.
- Context: The daemon endpoint already exists and shells through canonical `caco file add`; this slice does not invent a new protocol.

## After state

- Failing tests: none observed in focused validation.
- Relevant metrics: ConnectionManager now has a top-level/testable payload builder and `uploadFileCache(...)` method posting JSON to `/api/v1/file-cache` with project, filename, base64 content, MIME, title, description, tags, associations, and sender.
- Context: Android Files, QuickFile, and Agent Detail placeholders remain non-uploading; UI wiring is a future slice.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA
- Files touched: `companion/android/app/src/main/java/com/cacophony/companion/connection/ConnectionManager.kt`, `companion/android/app/src/test/java/com/cacophony/companion/FileCacheUploadClientSourceTest.kt`
- Tests: added focused unit/source test for payload shape, endpoint path, and placeholder non-upload invariants.
- Behavioural delta: Android now has a reusable client helper for later image upload UI wiring.

## Operator-takeaway

The Android side now has the upload client substrate needed to turn the existing image-sharing placeholders into real caco file uploads in a later slice.
