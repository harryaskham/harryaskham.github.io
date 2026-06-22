# Session summary — bd-c906e8: narrow/mobile viewport responsiveness test

## Goal

Add the responsive/mobile dimension to the Pico pane coverage (directive: native
conversation display parity with macOS/iOS/Android). Prove the pane does not gain
horizontal page overflow at a narrow mobile viewport even with stress content.

## Bead(s)

- `bd-c906e8` — narrow/mobile viewport responsiveness test (no horizontal overflow with stress content)

## Before state

- Failing tests: none. /pico is responsive-designed (viewport meta + @media
  rules; bubbles use overflow-wrap: anywhere, .pico-pre uses pre-wrap +
  overflow: auto), but no test proved no horizontal overflow at mobile width.

## After state

- Failing tests: none. New live subscenario opens a stress snapshot (a long
  unbreakable URL/token in an assistant body + a long tool command and long
  numeric output), resizes to 390x844 AFTER opening (open resets the viewport),
  and asserts documentElement.scrollWidth <= innerWidth + 2px while bubbles +
  composer render. Result: innerWidth 390, scrollWidth 390, overflowX 0. 2/2 clean.
- Vision pass at 390px confirmed: no overflow, the long URL/token wraps in-bubble,
  the long command/output wrap within their code blocks, layout clean + usable.
- caco-web bin 12; `--lib` 651; clippy clean.

## Diff summary

- Code/content commits: pending final squash SHA from the reintegration receipt.
- Files touched:
  - `crates/caco-web/src/bin/caco-web-observe.rs` — stress mock + responsive subscenario + eval + narrow screenshot.
- Tests: +1 live subscenario.
- Behavioural delta: test-only (responsive coverage); no product change.

## Embedded artefacts

- `web/screenshots/responsive-narrow.png` — the 390px mobile-layout capture.

## Operator-takeaway

Adds the responsive/mobile dimension: the conversation pane contains long
unbreakable content without breaking the page layout at a mobile viewport, both
asserted (no horizontal overflow) and vision-confirmed (clean wrapping). Note: the
caco-web-observe `open` action resets the viewport, so narrow tests must resize
AFTER opening.
