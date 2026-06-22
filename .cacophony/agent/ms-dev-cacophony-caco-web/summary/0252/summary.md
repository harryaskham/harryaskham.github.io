# Session summary — bd-7e561e: Pico streaming preserves scroll position

## Goal

While building a fast-streaming WebSocket test for the Pico transcript, I found a
real bug: during streaming, an operator who scrolled UP to read earlier
conversation was yanked back to the TOP on every streamed frame, making history
unreadable while the agent streams. Fix the scroll-anchoring so the reading
position is preserved (while keeping bottom auto-follow).

## Bead(s)

- `bd-7e561e` — caco-web Pico: streaming yanks history-reading operator to top every frame (scroll-position not preserved)

## Before state

- Failing tests: none.
- `renderPicoSnapshot` rebuilds the transcript with `host.innerHTML = …`, which
  resets `scrollTop` to 0, and only restored position in the at-bottom case
  (`if (wasNearBottom) host.scrollTop = host.scrollHeight`). Scrolled-up readers
  were reset to the top on every frame.

## After state

- Failing tests: none.
- `renderPicoSnapshot` captures `const prevScrollTop = host.scrollTop` before the
  innerHTML swap and, after it, follows the bottom when at the bottom OR restores
  the prior position (`else host.scrollTop = prevScrollTop`). Bottom auto-follow
  is unchanged; reading position is now stable during streaming.
- caco-web `--lib` 648 (new static guard); clippy clean (all-targets); live
  pico-pane scenario clean.

## Diff summary

- Code/content commits: pending final squash SHA from the reintegration receipt.
- Files touched:
  - `crates/caco-web/static/app.js` — renderPicoSnapshot scroll-position
    preservation.
  - `crates/caco-web/src/tests.rs` — guard
    `pico_streaming_preserves_scroll_position_bd_7e561e`.
- Tests: +1 static source guard.
- Behavioural delta: scrolled-up history reading is no longer interrupted by
  streamed frames.

## Operator-takeaway

Validation note for the next agent: I prototyped a live Playwright streaming
scroll test but it was too fragile — the standalone `/pico` fixture page scrolls
the PAGE (the transcript div isn't the scroll container there; only the bounded
Agent Detail pane in the real dashboard is), and rAF-coalesced streaming makes
live `scrollTop` timing racy. The robust, deterministic check for this DOM-render
behaviour is a static source guard (matching the existing bd-68be53 / bd-931b16
Pico needle-guard pattern), not a live scroll assertion. The bug itself was real
and found *because* I tried to write the streaming test.
