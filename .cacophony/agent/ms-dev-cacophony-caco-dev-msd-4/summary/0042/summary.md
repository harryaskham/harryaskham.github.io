# Session summary — macOS sidebar row clicks

## Goal

Fix the latest native macOS app regression where Tendril sidebar row clicks still left the app visibly stuck on the Status pane.

## Bead(s)

- `bd-a973d0` — [macOS visual QA] Latest desktop app sidebar row clicks still remain on Status

## Before state

- Failing tests: none at claim time.
- Relevant metrics: Tendril QA evidence on the latest app build showed clicks on Agents, Beads, Messages, Controls, Operations, Diagnostics, Admin, and Workspace all kept the Status pane selected.
- Context: the sidebar rows were implemented as plain Buttons inside a List with a selection binding; source smoke only proved an action fallback existed, not that the row used native sidebar selection semantics.

## After state

- Failing tests: none in the lightweight macOS validation path.
- Relevant metrics: `scripts/macos-app-pane-navigation-smoke.sh`, `scripts/macos-app-command-palette-smoke.sh`, `scripts/macos-app-window-chrome-smoke.sh`, `just --dry-run macos-app-validate`, `cargo fmt --all -- --check`, `bash -n scripts/macos-app-pane-navigation-smoke.sh`, and `git diff --check` passed.
- Context: sidebar rows now use `NavigationLink(value:)` with a full-width label and retain an explicit simultaneous tap fallback that updates `navigation.selection` and visible feedback.

## Diff summary

- Commits: `7b801b121`
- Files touched: `companion/macos/Sources/Cacophony/Views/RootView.swift`, `scripts/macos-app-pane-navigation-smoke.sh`
- Tests: strengthened the pane navigation source smoke to require native NavigationLink row semantics plus the explicit fallback.
- Behavioural delta: row clicks should now participate in SwiftUI sidebar/list navigation while still forcing selection updates when the sidebar gesture path is flaky.

## Operator-takeaway

The fix replaces inert-prone plain Button sidebar rows with native NavigationLink rows plus a fallback selection assignment, targeting exactly the visual QA symptom where row clicks did not leave Status.
