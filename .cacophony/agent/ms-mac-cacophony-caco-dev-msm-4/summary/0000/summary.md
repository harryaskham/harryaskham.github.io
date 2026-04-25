# Session summary — bd-4f63a8 macOS UI acceptance harness

## Goal
Add a deterministic, runner-safe native macOS UI/visual acceptance harness so release/manual lanes can capture a representative app path without running expensive UI automation on every push.

## Bead(s)

- `bd-4f63a8` — [macOS excellence] UI automation and visual regression harness

## Before state

- The native macOS app had build, install, smoke, package, and release workflows, but no command that launched the app and captured a deterministic UI artifact.
- The manual/tag-gated macOS CI job packaged artifacts but did not exercise the rendered SwiftUI app path beyond smoke binaries.

## After state

- Added `companion/macos/Scripts/ui-acceptance.sh`, which launches the built app, opens the offline Settings path, captures `dist/macos-ui/settings.png`, verifies key Settings labels in source, and writes `dist/macos-ui/summary.json` with bytes and SHA-256.
- Added `just macos-app-ui-acceptance` to build if needed and run the harness on macOS only.
- Wired the command into the already manual/tag-gated macOS app CI job and documented the workflow in the macOS README/development guide.

## Diff summary

- Commit: `2e48781b6` after stale-branch replay.
- Files touched: `justfile`, `.github/workflows/ci.yml`, `companion/macos/Scripts/ui-acceptance.sh`, `companion/macos/README.md`, `docs/macos-development.md`.
- Tests: no Rust tests required for this shell/docs/UI-harness slice.
- Validation: `just --list`; `bash -n companion/macos/Scripts/ui-acceptance.sh`; `./docs/validate-pages.sh`.
- Behavioural delta: manual/tag macOS lanes now produce a visual-regression screenshot and JSON metadata for the app's Settings path.

## Operator-takeaway

The native macOS app now has a first deterministic visual acceptance artifact path, giving operators something concrete to inspect in gated CI without spending scarce macOS runner time on every push.
