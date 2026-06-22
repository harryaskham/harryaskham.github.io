# Session summary — bd-1620d4: durable caco-web pico observe scenario

## Goal

Add repeatable browser-observation evidence for caco-web pico parity so future agents can validate the native browser Pico session without requiring a live pico worker. The scenario should fail if the browser falls back to terminal/PTY or misses core Android/iPhone parity widgets.

## Bead(s)

- `bd-1620d4` — [pico] caco-web: durable live observe scenario + screenshots for Android/iPhone parity.

## Before state

- `caco-web-observe` had a primitive `pico-pane` scenario, but help text did not advertise it and it depended on finding a live pico agent.
- There was no deterministic fixture covering the now-landed parity surface: shared core renderer, dialogs, model picker, inline images, notification/widget footer, composer, and no-terminal assertion.

## After state

- `caco-web-observe --scenario pico-pane` is deterministic and fixture-backed.
- The scenario opens `/pico`, mounts `CacoPicoAgentView.mountFixture`, and asserts native pane/transcript/composer/no-terminal/dialog/model/image/notice/widget/suggestions presence.
- Scenario artifacts are bounded and stored under the requested output directory.
- The latest run is console-clean: Total messages 0, Errors 0, Warnings 0.
- caco-web validation passes: 639 tests.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA.
- Files touched:
  - `crates/caco-web/src/bin/caco-web-observe.rs` — deterministic `pico-pane` scenario and updated help.
  - `crates/caco-web/static/app.js` — fixture renderer helper and standalone bootstrap cleanups.
  - `crates/caco-web/static/pico-agent-view.js` — `mountFixture` helper.
  - `crates/caco-web/static/pico.html` — favicon to keep standalone scenario console-clean.
  - `crates/caco-web/src/tests.rs` — include pico assets in window export / ID audits where needed.
  - `.cacophony/agent/.../summary/pending/web/` — scenario logs/screenshots/snapshot.
- Tests: existing caco-web library suite + live scenario run.
- Behavioural delta: no production-user behavior change except cleaner standalone `/pico`; this adds a reusable validation harness and fixture path.

## Embedded artefacts

- `web/pico-observe.log` — full scenario transcript/assertion output.
- `web/pico-observe-server.log` — dev server log.
- `web/screenshots/page-2026-06-15T02-39-52-662Z.png` — final scenario screenshot.
- `web/page-snapshots/page-2026-06-15T02-39-42-554Z.yml` — page snapshot.
- `web/validation.txt` — command/results summary.

## Operator-takeaway

caco-web now has a repeatable browser-side Pico parity proof: one command exercises the standalone native pane, verifies no terminal fallback, checks dialogs/model picker/images/notifications/widgets/composer, and captures console-clean screenshots for review.
