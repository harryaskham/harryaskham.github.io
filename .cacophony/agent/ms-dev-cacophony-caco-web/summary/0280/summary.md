# Session summary — bd-2eced8: consolidate duplicate Pico notification rendering to the TUI-faithful strip

## Goal

Fix a self-introduced regression found via live vision inspection on session
recreate: the Pico snapshot notifications were rendering TWICE (a dedicated strip
plus duplicate footer chips).

## Bead(s)

- `bd-2eced8` — snapshot notifications render twice; consolidate to the strip
- Root cause spans `bd-963b81` (added footer `pico-foot-notice` chips, earlier) and
  `bd-056cf7` (added the dedicated `pico-notif` strip later, after an audit that
  missed the footer rendering) — both this agent.

## Before state

- Failing tests: none (each rendering passed its own test independently). But the
  rendered pane showed notifications twice: footer chips ("warning: low memory")
  AND a dedicated strip ("[warning] low memory"). The footer was crowded (10
  chips). The bd-056cf7 audit grepped `snapshot.notifications`/`pico-notification`
  and missed the footer's `snap.notifications`/`pico-foot-notice` rendering.

## After state

- Failing tests: none. Notifications render once, in the dedicated kind-styled
  strip (renderPicoNotifications), matching the shared TUI which renders
  notifications in a dedicated area (notification_lines_themed) SEPARATE from its
  footer line. Removed the footer pico-foot-notice rendering + dead CSS; footer
  de-crowded to title/model/effort/status. Updated the main PICO_ASSERT_EVAL
  notices selector (.pico-foot-notice -> .pico-notif) and the bd-963b81 parity
  guard. Vision-confirmed single rendering. Live pico-pane 3/3 clean.
- caco-web bin 12; `--lib` 656; clippy clean.

## Diff summary

- Code/content commits: pending final squash SHA from the reintegration receipt.
- Files touched:
  - `crates/caco-web/static/app.js` — removed footer notification chips from renderPicoFooter.
  - `crates/caco-web/static/style.css` — removed dead .pico-foot-notice CSS.
  - `crates/caco-web/src/bin/caco-web-observe.rs` — main eval notices selector -> .pico-notif.
  - `crates/caco-web/src/tests.rs` — bd-963b81 guard needles -> strip rendering.
- Tests: 0 net (selectors repointed); behavioural fix.
- Behavioural delta: notifications render once (dedicated strip), footer de-crowded.

## Embedded artefacts

- `web/screenshots/notifications-consolidated.png` — the fixed pane: strip only, footer clean.

## Operator-takeaway

The profile's "drive the real app, look at the rendered UI" duty caught a
duplication that passed all functional tests (each rendering was individually
correct, but together they double-rendered). On recreate, a fresh live vision
pass is worth doing even when unit/scenario tests are green. Lesson for future
parity audits: grep for the DATA field (snap.notifications) across ALL render
functions, not just the expected class names, before concluding a field is unrendered.
