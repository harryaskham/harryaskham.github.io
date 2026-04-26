# Session summary — Cacophony Canary macOS target

## Goal

Add an operator-facing `Cacophony Canary.app` target that is refreshed alongside the agent-managed Test app, giving Harry a latest-build desktop app he can use knowingly while keeping stable production `Cacophony.app` untouched.

## Bead(s)

- `bd-680d62` — [macOS QA infra] Add Cacophony Canary app target

## Before state

- Failing tests: none known.
- Relevant metrics: macOS helper supported production `Cacophony.app` and isolated `Cacophony Test.app` only. Test used bundle id `com.cacophony.macos.test`, socket `/tmp/cacophony-macos-test.sock`, and no-Keychain QA mode.
- Context: Harry wanted a third app identity: stable production for stability, Canary for latest builds, and Test for agent/Tendril automation. Canary needed to be refreshable even if he happened to be using it.

## After state

- Failing tests: none known.
- Relevant metrics: added Canary identity with bundle id `com.cacophony.macos.canary`, display name `Cacophony Canary`, command socket `/tmp/cacophony-macos-canary.sock`, and Keychain service `com.cacophony.macos.canary`.
- Context: `just macos-app-test-install` still launches Test, but a full Test install also refreshes the sibling Canary bundle under the same stable QA root. `just macos-app-canary-install`, `macos-app-canary-relaunch`, `macos-app-canary-hot-swap`, and `macos-app-canary-paths` provide explicit Canary operations.

## Diff summary

- Commits: `45aa819d96e2440bc01c32a2bb17c522455cb1f4`
- Files touched: `companion/macos/Sources/CacophonyKit/AppRuntimeIdentity.swift`, `scripts/macos-app-qa-launch.sh`, `scripts/macos-app-focus-pane.sh`, `scripts/macos-app-provenance.sh`, `scripts/macos-app-pane-navigation-smoke.sh`, `justfile`, `README.md`, `AGENTS.md`, `companion/macos/README.md`, `companion/macos/PARITY.md`, `docs/macos-development.md`, `docs/macos-development.html`, `.cacophony/profiles/caco-macos.md`, `KeychainStore.swift`.
- Tests: `bash -n` on touched shell scripts; `just macos-app-command-palette-smoke`; `just macos-app-pane-navigation-smoke`; `just macos-app-window-chrome-smoke`; `just macos-app-swift-syntax` (Nix Swift fallback, parsed 42 Swift files); `docs/validate-pages.sh` (1695 passed, 0 warnings, 0 failed); `git diff --check`; `scripts/macos-app-qa-launch.sh --print-paths`; `scripts/macos-app-qa-launch.sh --canary --print-paths`; `just --list` canary recipe check.
- Behavioural delta: production `/Applications/Cacophony.app` remains untouched; Test full installs may quit/replace Test and refresh Canary; Canary has its own socket and Keychain namespace and may be replaced while in use by design.

## Operator-takeaway

Harry now has three clear native app lanes: production `Cacophony.app` for stability, `Cacophony Canary.app` for latest builds that may be replaced during ongoing QA, and `Cacophony Test.app` for isolated agent automation.
