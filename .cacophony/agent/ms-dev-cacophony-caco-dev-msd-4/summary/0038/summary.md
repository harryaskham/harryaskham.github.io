# Session summary — macOS local daemon auto-connect

## Goal

Make a fresh macOS app launch connect to the healthy local daemon when the canonical node token is available, instead of remaining on a generic offline card.

## Bead(s)

- `bd-81e8ee` — [macOS visual QA] App remains in offline state while local daemon is healthy

## Before state

- Failing tests: none at claim time.
- Relevant metrics: Tendril evidence showed `caco status` healthy on ms-mac while the native app stayed offline after Retry, Cmd-R, and toolbar refresh.
- Context: the app only auto-connected from Keychain on launch, so first-run/fresh visual QA sessions with a valid local token file could still start offline.

## After state

- Failing tests: none in the lightweight validation path.
- Relevant metrics: `scripts/macos-app-pane-navigation-smoke.sh`, `scripts/macos-app-command-palette-smoke.sh`, `scripts/macos-app-window-chrome-smoke.sh`, `just --dry-run macos-app-validate`, `cargo fmt --all -- --check`, `bash -n scripts/macos-app-pane-navigation-smoke.sh`, and `git diff --check` passed.
- Context: launch now falls back to `~/.cacophony/tokens/node.token` for `127.0.0.1:11100` when no Keychain profile exists, and shows an actionable Settings/token error when neither source is present.

## Diff summary

- Commits: `40e8eae44`
- Files touched: `companion/macos/Sources/Cacophony/App/DaemonState.swift`, `scripts/macos-app-pane-navigation-smoke.sh`
- Tests: extended the macOS source smoke to assert local-token auto-connect fallback and actionable missing-token copy.
- Behavioural delta: fresh local macOS launches can connect to a healthy daemon without manual Settings setup when the canonical token file is present.

## Operator-takeaway

The macOS app now treats the canonical local daemon token as a first-run auto-connect path, so offline screenshots on a healthy local daemon should become real connection/config failures rather than silent missing-Keychain state.
