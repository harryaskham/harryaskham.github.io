# Session summary — caco-web SSE transient-drop grace

## Goal

Investigate and reduce false disconnected banners in caco-web when the browser sees short-lived Server-Sent Events errors between the dashboard and local daemon.

## Bead(s)

- `bd-3ffce9` — Investigate SSE connection drops between webui and local daemon

## Before state

- Failing tests: none known.
- Relevant metrics: code inspection showed the SSE error reporting threshold was effectively zero seconds: `sse.onerror` immediately called `setConnectionStatus(... 'disconnected')`, showed the lost-connection toast, closed the stream, and scheduled exponential reconnect. Backoff started at about 1s plus jitter and capped at 30s; the independent liveness probe waits two 15s ticks, about 30s, before forcing a reconnect.
- Context: transient EventSource errors could flash a disconnected state even when SSE had just opened or delivered an event and the natural reconnect was likely to recover quickly.

## After state

- Failing tests: none in caco-web validation.
- Relevant metrics: added a 15s transient-error grace after recent SSE open/activity. Playwright forced a recent SSE error and observed the connection pill remain `Connected` immediately with `graceRemainingMs: 15000` and retry count incremented; console had zero errors/warnings. `cargo check -p caco-web --all-targets` passed; `cargo test -p caco-web --lib` passed with 284 tests.
- Context: if the stream does not recover before the grace expires, the existing disconnected/backend-unavailable reporting still fires; manual reconnect clears the grace window.

## Diff summary

- Commits: `84e4d2b6d`
- Files touched: `crates/caco-web/static/app.js`, `crates/caco-web/src/tests.rs`
- Tests: +1 regression test / -0 / flipped 0
- Behavioural delta: recent healthy SSE sessions now tolerate short EventSource errors without immediately alarming the operator, while longer failures still degrade visibly and continue using the existing backoff/liveness recovery paths.

## Embedded artefacts

- `/tmp/caco-web-bd-3ffce9-grace-185649-playwright.log` — Playwright proof of the forced recent-SSE error, 15s grace, and zero console errors.
- `.playwright-cli/page-2026-04-26T17-57-16-664Z.png` — screenshot from the caco-web SSE grace smoke.

## Operator-takeaway

The investigation found the dashboard had no debounce at all for SSE errors. A bounded 15s grace now filters brief EventSource churn without hiding real sustained connectivity loss.
