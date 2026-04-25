# Session summary — bd-310e83 macOS first-run onboarding

## Goal
Make the native macOS app's disconnected Settings state self-explanatory for first-time users by documenting the local daemon token path and providing a safe helper to populate local daemon defaults.

## Bead(s)

- `bd-310e83` — [macOS excellence] First-run onboarding and token discovery helper

## Before state

- Settings supported named daemon profiles and Keychain persistence, but first-run guidance only said to save profiles.
- Users had to already know where the local daemon token lived and manually paste it without in-app path/copy guidance.
- The smoke runner did not cover token-path constants.

## After state

- Added a First-run setup card to Settings explaining `~/.cacophony/tokens/node.token`.
- Added controls to use local daemon defaults, copy the expanded token path, and show the token folder in Finder.
- Added `DaemonConfig.defaultTokenPath`, `expandedDefaultTokenPath`, and `discoverLocalNodeToken()` so token discovery is centralized and testable.
- Extended the macOS smoke runner to verify token path guidance, raising the check count to 51.

## Diff summary

- Commit: `1294cce22` after stale-branch replay.
- Files touched: `companion/macos/Sources/Cacophony/Views/SettingsView.swift`, `companion/macos/Sources/CacophonyKit/Models/DaemonConfig.swift`, `companion/macos/Sources/CacophonyKitSmoke/main.swift`.
- Tests: +2 smoke checks for token path docs/expansion.
- Validation: `just macos-app-test`; `just --list`; `./docs/validate-pages.sh`.
- Behavioural delta: disconnected Settings now guides local-token discovery without requiring secrets in source or assuming remote profiles.

## Operator-takeaway

A fresh macOS app install now tells the operator where the local node token is, can load it when present, and can copy/open the path for manual setup.
