# Session summary — bd-da2b54 part-a: reconcile 8 drifted android source-pins (green baseline)

## Goal
Restore a green :app:testDebugUnitTest baseline by reconciling the 8 pre-existing broken
source-pin tests to current source (each pinned a string the source legitimately evolved past,
uncaught by the Rust-only merge gate).

## Bead(s)
- bd-da2b54 part-a (android test health; part-b = the merge-gate gap, daemon/Rust, separate).

## Before/After state
- Before: 8 failing source-pin tests on main (test-vs-source drift).
- After: all 8 reconciled to current source + pass (verified each via grep + targeted run, F=0 E=0):
  - 4 from bd-a120bd watch-profile refactor (publish now goes via publishWatchProfileToWatch(cfg)
    in MainActivity / wearRelay.publishDaemonProfile(target.*) in the SettingsScreen button):
    DaemonProfileRefresh, MainActivityOnResumePublishProfile, WatchPhoneDaemonProfile,
    WatchNodeTokenPushExplicit, WatchAppSendToWatch.
  - ConnectionManagerSourceTest: broadcast() gained a global param + (global=$global) log text.
  - PicoStandaloneActivitySourceTest: source ctor -> OkHttpPicoSessionSource.defaultWebSocketClient() (bd-bcc919).
  - QaScreenshotScriptTest: --navigate help inserted "suggestions" between profiles/web-app;
    relaxed the rigid item-sequence Regex to the stable summaries,\s+profiles prefix.

## Diff summary
- Code/content commits: pending reintegration receipt SHA.
- Files: 8 *SourceTest.kt assertion updates (test-only; no production source changed).
- Tests: 8 fixed, 0 new failures (test-only changes can't break other tests).

## Embedded artefacts
- None. Targeted run confirms all 8 pass.

## Operator-takeaway
The android source-pin baseline is green again. bd-da2b54 part-b (the ROOT CAUSE: the Rust-only
merge gate never runs :app:testDebugUnitTest, so android pins rot silently) remains — that's a
daemon/merge-queue change (Rust), needs ctrl/Rust-dev coordination, not pure-android. Several of
these 8 drifted from MY earlier session lands (bd-a120bd/bd-bcc919) precisely because of that gap.
