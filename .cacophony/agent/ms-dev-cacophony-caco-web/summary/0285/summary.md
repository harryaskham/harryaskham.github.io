# Session summary — bd-21b6a2: web pico bash-mode composer UX (! / !!)

## Goal

Implement the WEB per-surface bash-mode UX child of the bd-b7c70b cross-surface
bash-mode umbrella (iOS bd-caf79d + watchOS bd-475c65 done; web had 0 bashMode
refs). caco-android-msd-0 (the umbrella coordinator) routed it to me as the
caco-web/Pico specialist. `!cmd` (visible) / `!!cmd` (hidden) should route to Pi's
RPC bash with a visible bash-mode input affordance.

## Bead(s)

- `bd-21b6a2` — Picophony bash-mode: web pico composer UX (filed + claimed +
  closed this session). Parent: bd-b7c70b. Siblings: bd-caf79d (iOS), bd-475c65
  (watchOS, landed).

## Before state

- The web pico composer sent `!cmd` as a literal PROMPT (no bash-mode), and had no
  visual bash-mode affordance — 0 bashMode refs in app.js.

## After state

- ROUTING was already INHERITED: the shared `parse_composer_line` (bd-006d53)
  returns `RpcCommand::Bash{command, exclude_from_context}` for `!`/`!!`, and the
  web's `picoComposerLine` routes a command action via `commandLine` — so `!`/`!!`
  already reach Pi's RPC bash through the wasm parser. No wasm/routing change
  needed (verified by the live test below).
- ADDED the bash-mode INPUT AFFORDANCE: `picoComposerInput` toggles
  `pico-bash-mode` (any `!` prefix) + `pico-bash-mode-hidden` (`!!`) on the
  composer input; style.css shifts it to monospace + an accent tint (visible) /
  muted italic (hidden), mirroring iOS PicoAgentView (bd-caf79d).
- TESTS: a needle guard (`pico_web_bash_mode_composer_affordance_bd_21b6a2`) +
  a deterministic live mock subscenario (`run_pico_bash_mode_subscenario`):
  asserts `!`->visible-style, `!!`->hidden-style, plain->no-style, submit-clears,
  AND that the mock /session server RECEIVED a `{"type":"bash",...}` command for
  the visible `!` and one with `"excludeFromContext":true` for the hidden `!!`
  (proving the routing, not just the styling). pico-pane scenario now 60 results,
  2/2 deterministic.

## Diff summary

- Code commit: pending (final landed squash SHA from the reintegration receipt).
- Files: crates/caco-web/static/{app.js,style.css}, crates/caco-web/src/{tests.rs,
  bin/caco-web-observe.rs}. No wasm change (routing inherited).
- Tests: +1 needle guard, +1 live subscenario (deterministic, 2/2). caco-web --lib
  queued.

## Operator-takeaway

The web pico composer now has bash-mode parity with iOS/watchOS: `!cmd` runs a
shell command into the conversation and `!!cmd` runs it hidden, with the input
visibly shifting to monospace bash-mode styling. The routing was already free
(shared parse_composer), so the slice was the input affordance + the proof-test
that `!`/`!!` actually reach Pi's RPC bash. Closes the web surface of the
bd-b7c70b cross-surface bash-mode umbrella.
