# Session summary — bd-47dc20: add Android timeline view

## Goal

Expose the daemon's aggregated timeline in the Android companion as a real
mobile surface, with cluster/project scope via the existing project selector,
touch-friendly scrolling, and simple date/range filtering.

## Bead(s)

- `bd-47dc20` — Create timeline view for Android app

## Before state

- The Android companion had no timeline tab or timeline screen.
- The daemon already exposed `/api/v1/timeline`, but the Android app did not
  fetch or render it.
- Operators could browse Feed on mobile, but not the aggregated timeline view
  that now exists in TUI/daemon surfaces.

## After state

- Added a new `Timeline` bottom-nav tab to the Android companion.
- Added `ConnectionManager.fetchTimeline(...)` plus JSON models for the daemon
  timeline envelope.
- Added `ui/timeline/TimelineScreen.kt`, which:
  - loads cluster timeline by default
  - switches to project scope automatically when the app's project picker is
    set
  - offers mobile range chips (`24h`, `48h`, `7d`)
  - renders timeline cards with project, actor, relative time, and event kind
  - supports pull-to-refresh and a floating refresh action
- Verified the phone app still compiles with the new screen wired into the main
  tab flow.

## Diff summary

- Files touched:
  - `companion/android/app/src/main/java/com/cacophony/companion/MainActivity.kt`
  - `companion/android/app/src/main/java/com/cacophony/companion/connection/ConnectionManager.kt`
  - `companion/android/app/src/main/java/com/cacophony/companion/ui/timeline/TimelineScreen.kt`
- Validation:
  - `cd companion/android && nix develop . -c gradle :app:compileDebugKotlin --console=plain`
- Behavioural delta:
  - Android now has a first-class timeline surface instead of relying only on
    Feed for mobile historical context.

## Operator-takeaway

This lands the Android timeline MVP cleanly on top of the already-shipped
backend: the app now exposes the timeline as a native mobile tab, scoped by the
existing project picker and backed by the same daemon endpoint as other
surfaces. It is a practical mobile view now, not just a future design note.
