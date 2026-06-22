# Session summary — bd-f5b97a: Pico snapshot title footer chip

## Goal

Continue the caco-web Pico native-display polish by surfacing `AgentViewSnapshot.title` in the browser footer. The shared model already carries a session/window title, and native surfaces use that context to orient operators.

## Bead(s)

- `bd-f5b97a` — [pico] caco-web: render Pico snapshot title as native footer context.

## Before state

- The mock snapshot included `title: "mock pico"`, but caco-web ignored `snapshot.title`.
- The footer showed model/context/activity/status/notifications/widgets, but not the session title context.

## After state

- `renderPicoFooter` renders `snapshot.title` as a `pico-foot-title` chip.
- The chip has dedicated styling aligned with the existing footer chip family.
- The mock websocket scenario asserts `titleChip === "mock pico"`.
- Validation is green: caco-web-observe 12 tests; caco-web lib 645 tests.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA.
- Files touched:
  - `crates/caco-web/static/app.js` — title chip rendering.
  - `crates/caco-web/static/style.css` — title chip styling.
  - `crates/caco-web/src/bin/caco-web-observe.rs` — scenario final assertion.
  - `crates/caco-web/src/tests.rs` — source guard.
  - `.cacophony/agent/.../summary/pending/web/title-mock-test/` — scenario evidence.
- Tests: +1 caco-web source guard for title chip rendering.
- Behavioural delta: Pico sessions now show snapshot title context in the native browser footer.

## Embedded artefacts

- `web/title-mock-test/pico-title-observe.log` — scenario log with titleChip assertion.
- `web/title-mock-test/screenshots/page-2026-06-16T09-16-34-489Z.png` — final screenshot.
- `web/title-mock-test/page-snapshots/page-2026-06-16T09-16-16-068Z.yml` — page snapshot.
- `web/validation.txt` — command/results summary.

## Operator-takeaway

The browser Pico pane now exposes the session title from the shared snapshot, improving orientation without adding visual noise.
