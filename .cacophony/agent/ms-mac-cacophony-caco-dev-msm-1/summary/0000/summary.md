# Session summary — macOS offline pane-specific context

## Goal

Fix the Tendril-discovered disconnected-mode UX issue where sidebar navigation appeared to leave the detail area stuck on the Messages offline state, making full-surface visual QA and operator navigation misleading when the daemon is unreachable.

## Bead(s)

- `bd-9d0756` — `[macOS visual QA] Offline pane selection should still update context`
- Found by: `bd-68593c` — `[macOS visual QA] Full-surface Tendril UX sweep`

## Before state

- Failing tests: none known in the targeted macOS app lane.
- Relevant metrics: Tendril sweep screenshots showed every offline pane capture looking like the Messages pane even after sidebar/shortcut navigation.
- Context: Pane-specific detail views each embedded `NotConnectedView`, so disconnected state could obscure which pane was selected.

## After state

- Failing tests: none observed in targeted validation.
- Relevant metrics: `swift build` passed; `nix build .#cacophony-macos-app -L` passed with `CacophonyKitSmoke: OK (53 checks)`.
- Context: Root detail now renders a pane-specific offline placeholder for every non-Settings pane, preserving the selected pane label, icon, tagline, retry action, settings action, and shortcut hints.

## Diff summary

- Commits: current branch commit for `bd-9d0756`.
- Files touched: `companion/macos/Sources/Cacophony/Views/RootView.swift`.
- Tests: no smoke-count change; app build and smoke suite passed.
- Behavioural delta: disconnected mode now confirms the selected pane instead of visually appearing stuck on Messages.

## Operator-takeaway

Offline app navigation is now testable and trustworthy: operators can move across panes even when the daemon is down and still see which pane context they are in.
