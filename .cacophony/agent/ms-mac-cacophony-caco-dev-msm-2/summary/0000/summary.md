# Session summary — macOS global pane shortcut routing

## Goal

Fix the native macOS visual-QA regression where Tendril captures showed every ⌘1 through ⌘0 pane shortcut leaving the app visibly stuck on Status after a minimal-copy install.

## Bead(s)

- `bd-84be3e` — [macOS visual QA] Pane shortcuts leave app stuck on Status after minimal-copy install

## Before state

- Failing tests: no automated Swift/UI test failure; the failing evidence was visual QA screenshots from `.cacophony/agent/ms-mac-cacophony-caco-dev-msm-1/summary/0062/screenshots/`.
- Relevant metrics: installed `/Applications/Cacophony.app` remains stale versus this checkout and lacks newer command-socket symbols.
- Context: shortcuts were registered through SwiftUI commands/sidebar links, but visual QA still observed no visible pane switch for cmd+1 through cmd+0.

## After state

- Failing tests: none from static/source validation; no local Swift build was run by design.
- Relevant metrics: `just macos-app-provenance` confirms installed app is stale (expected) and should be replaced by cloud-built artifact before live visual QA.
- Context: plain command-digit pane jumps now route through a small AppKit local key monitor attached to the main RootView, consuming only plain ⌘0–⌘9 and updating `AppNavigation.selection` directly.

## Diff summary

- Commits: `51da5beb3`
- Files touched: `companion/macos/Sources/Cacophony/App/CacophonyApp.swift`, `companion/macos/README.md`
- Tests: `git diff --check origin/main..HEAD`; static Python source assertions for the AppKit key monitor and digit-to-pane map; `just macos-app-provenance` (expected stale-app failure, no local Swift build)
- Behavioural delta: the app keeps the SwiftUI menu shortcuts, but now has deterministic global routing for plain ⌘1–⌘0 even when focus is inside pane content, with visible feedback and stale error clearing.

## Operator-takeaway

This is a source fix for the shortcut-routing failure; the installed app is still stale, so the next proof should come from the cloud build/install visual-QA path rather than a local ms-mac Swift build.
