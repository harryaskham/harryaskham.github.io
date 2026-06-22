# Session summary — bd-056cf7: render the Pico snapshot notifications field (TUI parity)

## Goal

Close a high-value native-parity gap found during a quiet-window coverage audit:
the web Pico pane rendered the snapshot's `notifications` field nowhere, while
the TUI renders it — so real pi session notifications (errors/warnings) were
invisible in the web Pico pane.

## Bead(s)

- `bd-056cf7` — Pico snapshot notifications field rendered in the TUI but invisible in the web pane (P2)
- Mirrors shared TUI `render.rs` notification_lines_themed (epic `bd-93f302`).

## Before state

- Failing tests: none. AgentViewSnapshot.notifications (Vec<(String,String)> =
  (kind, message)) flows through the wasm snapshot to the web (bd-b11d95 guard
  already lists the field; committed wasm carries it), but app.js only ever read
  state.notifications (the dashboard-chrome notification view), never
  picoState.snapshot.notifications. The TUI renders the last 3 as "[{kind}] {msg}"
  styled error->red / warning->warning / else dim. The web showed none.

## After state

- Failing tests: none. New renderPicoNotifications() renders the last 3
  snapshot.notifications as a pinned strip (new #agent-pico-notifications element
  between the transcript and footer), each "[{kind}] {msg}" with kind styling
  (.pico-notif-error / -warning / -info), XSS-escaped, hidden when empty —
  mirroring the TUI surface. Wired into renderPicoSnapshot. New live subscenario
  (4 notifications -> last 3 render, kinds styled, oldest windowed out, <full>/<ok>
  escaped) 2/2 clean; static guard pins the render + CSS seam.
- caco-web bin 12; `--lib` 655 (+1 guard); clippy clean.

## Diff summary

- Code/content commits: pending final squash SHA from the reintegration receipt.
- Files touched:
  - `crates/caco-web/static/app.js` — renderPicoNotifications + #agent-pico-notifications element + wiring.
  - `crates/caco-web/static/style.css` — .agent-pico-notifications / .pico-notif-* styling.
  - `crates/caco-web/src/bin/caco-web-observe.rs` — notifications subscenario + eval.
  - `crates/caco-web/src/tests.rs` — static guard.
- Tests: +1 live subscenario, +1 static guard.
- Behavioural delta: real Pico notifications now visible in the web pane, at TUI parity.

## Embedded artefacts

- None (bounded).

## Operator-takeaway

The quiet-window coverage audit (re-checking every snapshot field against actual
web rendering) found that an entire snapshot field — notifications — was
TUI-rendered but web-invisible, distinct from the dashboard-chrome
state.notifications it was easy to confuse it with. Real pi errors/warnings are
now surfaced in the web Pico pane.
