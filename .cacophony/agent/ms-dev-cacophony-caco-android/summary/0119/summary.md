# Session summary — Android Settings embedded-daemon status

## Goal

Add a safe, non-executing Settings surface that records the Android embedded-daemon design posture before any daemon binary bundling, process launch, or storage-permission work begins.

## Bead(s)

- `bd-ea8dcc` — Android Settings: embedded daemon experimental status section
- parent context: `bd-372c92` — Android embedded caco daemon / FHS sharing spike

## Before state

- Failing tests: none known for this slice.
- Relevant metrics: the design note existed in `docs/design/daemon-embedding-feasibility.md`, but Android Settings had no visible indication of the future embedded daemon mode or FHS-sharing posture.
- Context: The parent is broad and high-complexity. This slice is explicitly UI/source-only.

## After state

- Failing tests: none observed in focused validation.
- Relevant metrics: Android Settings now includes an "Embedded daemon (experimental)" section stating that the daemon is not bundled yet, Android will default to an app-private `CACOPHONY_DIR` plus loopback HTTP API, WearOS stays remote-only, and raw FHS sharing is a future SAF/DocumentsProvider or power-user storage decision.
- Context: The section does not start processes, request storage permissions, or bundle native daemon binaries.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA
- Files touched: `companion/android/app/src/main/java/com/cacophony/companion/ui/settings/SettingsScreen.kt`, `companion/android/app/src/test/java/com/cacophony/companion/SettingsEmbeddedDaemonStatusSourceTest.kt`
- Tests: added focused Settings source test.
- Behavioural delta: operators can now see the embedded-daemon roadmap and non-default raw-FHS sharing stance in-app.

## Operator-takeaway

This does not embed caco yet; it makes the Android app explicit about the planned safe default so future implementation does not drift toward broad storage permissions or hidden process launch.
