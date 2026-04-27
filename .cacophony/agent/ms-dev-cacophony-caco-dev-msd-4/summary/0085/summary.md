# Session summary — isolated Ghostty terminal prototype

## Goal

Prototype a Ghostty/libghostty-backed terminal pane as an isolated macOS target, proving the adapter seam and host-managed pseudo-session shape without wiring it into the production Cacophony app.

## Bead(s)

- `bd-dd5935` — [macOS terminal] Prototype Ghostty-backed terminal pane in isolated target

## Before state

- Failing tests: none; this was a prototype slice.
- Relevant metrics: `companion/macos/Package.swift` exposed only the production `Cacophony` app, `CacophonyKit`, and `CacophonyKitSmoke`; no isolated Ghostty terminal target existed.
- Context: the prior investigation recommended keeping Ghostty/libghostty work outside production paths until focus, resize, cleanup, keyboard handling, and broker boundaries are proven.

## After state

- Failing tests: none in validation.
- Relevant metrics: added `CacophonyGhosttyTerminalPrototype` as a separate SwiftPM executable target with a guarded `GhosttyKit` import seam, fallback terminal canvas, focus state, synthetic resize placeholder, and a host-managed `/bin/zsh` pseudo-session driven through `Process`/`Pipe`.
- Context: production `Cacophony` app code does not reference the prototype target, and a new source-only smoke script pins that isolation contract.

## Diff summary

- Commits: `1dac62349`
- Files touched: `companion/macos/Package.swift`, `companion/macos/Sources/CacophonyGhosttyTerminalPrototype/GhosttyTerminalPrototypeApp.swift`, `scripts/macos-app-ghostty-prototype-smoke.sh`, `justfile`, `SPEC.md`, `README.md`, `AGENTS.md`, `companion/macos/README.md`, `docs/macos-development.md`, `docs/macos-development.html`
- Tests: `./scripts/macos-app-ghostty-prototype-smoke.sh`; `./scripts/macos-app-command-palette-smoke.sh`; `./scripts/macos-app-pane-navigation-smoke.sh`; `./scripts/macos-app-window-chrome-smoke.sh`; `./scripts/macos-app-chat-ui-smoke.sh`; `docs/validate-pages.sh`; `cargo test-small`.
- Behavioural delta: there is now an isolated desktop terminal-prototype executable for Ghostty experiments; routine production app validation includes a lightweight source-only guard for it.

## Operator-takeaway

The Ghostty terminal work now has a safe prototype target to iterate on without risking the production macOS app or bypassing the planned terminal session broker boundary.
