# Session summary — compact offline pane placeholder

## Goal

Polish the disconnected/offline pane placeholder discovered during Tendril visual QA so it no longer duplicates the global pane header or wastes vertical space in narrow windows.

## Bead(s)

- `bd-3a9178` — [macOS polish] Offline pane placeholder duplicates header context and wastes vertical space

## Before state

- Failing tests: none known for this macOS slice.
- Relevant metrics: `sidebar-polish-installed.png` showed a large pane header followed by a second offline card repeating the pane icon/title/tagline.
- Context: The disconnected placeholder was useful but visually redundant and cramped once the header became more responsive.

## After state

- Failing tests: none observed in targeted macOS validation.
- Relevant metrics: `swift build --jobs 1` passed; `CacophonyKitSmoke: OK (53 checks)` passed.
- Context: Offline context now uses `ViewThatFits` with regular and compact layouts, a generic daemon-connection icon, no duplicated pane tagline by default, and concise copy that preserves selected-pane context plus retry/settings actions.

## Diff summary

- Commits: `a4b67a722`
- Files touched: `companion/macos/Sources/Cacophony/Views/RootView.swift`
- Tests: +0 / -0 / flipped 0
- Behavioural delta: Disconnected panes now feel like a compact recovery state under the header instead of a second competing header.

## Operator-takeaway

This slice removes one of the most obvious “functional but unpolished” visual duplications in the native app, making offline/maintenance-window states calmer and more Mac-like.
