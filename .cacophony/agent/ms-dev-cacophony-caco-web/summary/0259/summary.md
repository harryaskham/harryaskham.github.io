# Session summary — bd-87a45b: web per-message send-failure + retry (native parity)

## Goal

Close a native conversation-display parity gap: the shared caco-picophony view
carries per-message send state (transcript_send_states: Pending/Sent/Failed) and
caco-tui + macOS/iOS/Android render a failed indicator + retry-by-resend, but the
caco-web Pico pane ignored it entirely — a failed user-message send was invisible
in the browser with no retry affordance.

## Bead(s)

- `bd-87a45b` — render per-message send-failure + retry (transcript_send_states native parity)

## Before state

- Failing tests: none. app.js had ZERO references to transcript_send_states; a
  Failed send rendered as an ordinary user bubble with no signal and no retry.

## After state

- Failing tests: none. renderPicoItem now reads
  picoState.snapshot.transcript_send_states[idx] for User items (the transcript_ts
  parallel-array pattern). Failed -> '⚠ failed to send (reason)' + a Retry button
  that re-sends the message text as a fresh prompt over the open /session socket;
  Pending -> subtle 'sending…'; Sent -> nothing. XSS-safe (escaped reason); pill
  radius via var(--radius-pill). New window.cacoPicoRetrySend handler.
- caco-web `--lib` 651 (+1 static guard); bin 12; clippy clean. Live pico-pane
  send-failure subscenario 2/2 clean (hasFailed, showsFailedText, showsReason,
  hasRetryButton, retryIsButton, noPending, userBubblePresent). Vision-confirmed
  the indicator + retry pill render cleanly and legibly.

## Diff summary

- Code/content commits: pending final squash SHA from the reintegration receipt.
- Files touched:
  - `crates/caco-web/static/app.js` — picoSendStateExtra, picoBubble `extra` slot,
    User-branch wiring, window.cacoPicoRetrySend.
  - `crates/caco-web/static/style.css` — .pico-send-state / .pico-send-failed /
    .pico-send-pending / .pico-retry-btn.
  - `crates/caco-web/src/bin/caco-web-observe.rs` — mock + live send-failure
    subscenario + screenshot.
  - `crates/caco-web/src/tests.rs` — bd-87a45b static guard; picoBubble-signature
    needle update; cacoPicoRetrySend window-export allowlist entry.
- Tests: +1 live subscenario, +1 static guard.
- Behavioural delta: failed user-message sends are now visible + retryable in the
  browser, matching every other surface.

## Embedded artefacts

- `web/screenshots/send-failure.png` — the rendered failed indicator + retry pill.

## Operator-takeaway

This was found by auditing which native-parity snapshot fields the web reads vs
ignores (the bd-b357f0 lesson generalized): transcript_send_states was carried by
the wasm but had no web consumer. The web Pico pane now has full send-state parity
with caco-tui/macOS/iOS/Android. Follow-up worth filing: the generic '.pico-role'
label (e.g. "YOU") is low-contrast across all bubbles — a separate legibility item.
