# Session summary — bd-931b16: Pico clean completion shows session ended

## Goal

Fix the caco-web Pico session status UX revealed by the mock websocket scenario: a clean session completion should not look like an error/disconnect.

## Bead(s)

- `bd-931b16` — [pico] caco-web: clean Pico session completion must show session ended, not disconnected.

## Before state

- When the mock `/session` WebSocket closed after a successful stream, `ws.onclose` overwrote the earlier `session ended` state with `disconnected`.
- Final screenshots showed a red/offline dot even though the session completed cleanly.

## After state

- `pi_exited` and shared PicoView exit state use a distinct `ended` mode with label `session ended`.
- `ws.onclose` preserves `ended` and only marks `disconnected` when the session did not already end cleanly.
- The ended state has a neutral/cyan dot style.
- The mock scenario now streams a `pi_exited` frame and asserts final status is exactly `session ended`.
- Validation is green: caco-web-observe 12 tests; caco-web lib 642 tests; scenario console-clean.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA.
- Files touched:
  - `crates/caco-web/static/app.js` — ended status handling and onclose guard.
  - `crates/caco-web/static/style.css` — ended dot styling.
  - `crates/caco-web/src/bin/caco-web-observe.rs` — mock pi_exited frame and final assertion.
  - `crates/caco-web/src/tests.rs` — source guard for ended behavior.
  - `.cacophony/agent/.../summary/pending/web/ended-mock-test/` — scenario evidence.
- Tests: +1 caco-web source test for clean completion status.
- Behavioural delta: clean finished Pico sessions now read as ended, not disconnected.

## Embedded artefacts

- `web/ended-mock-test/pico-ended-observe.log` — scenario run with final status assertion.
- `web/ended-mock-test/screenshots/page-2026-06-16T08-13-37-766Z.png` — final screenshot showing session ended.
- `web/ended-mock-test/page-snapshots/page-2026-06-16T08-13-26-621Z.yml` — page snapshot.
- `web/validation.txt` — validation commands/results.

## Operator-takeaway

The browser now distinguishes a successful completed Pico session from a broken connection, matching native-app expectations and avoiding false alarm red/disconnected styling.
