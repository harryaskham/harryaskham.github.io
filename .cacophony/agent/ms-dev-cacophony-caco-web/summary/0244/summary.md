# Session summary — bd-62c344: dynamic Pico slash command submits as backend prompt

## Goal

Prove dynamic slash commands from `AgentViewSnapshot.available_commands` do not only appear/click in suggestions, but also submit through caco-web Pico using the shared Picophony contract: unknown/dynamic slash commands are sent as prompt frames so the backend can route extension commands/templates/skills.

## Bead(s)

- `bd-62c344` — [pico] caco-web: dynamic slash command submit should prompt backend.

## Before state

- The live mock scenario proved `/deploy` appears and is clickable as a dynamic suggestion.
- It did not submit a dynamic slash command or assert the outbound frame.

## After state

- Added a minimal dynamic-command mock `/session` subscenario with `available_commands:["/deploy"]`.
- The subscenario submits `/deploy canary` through the real Pico composer.
- The mock backend asserts:
  - `{"kind":"command","type":"prompt","message":"/deploy canary"}`
- Existing dynamic suggestion click proof plus all main outbound frames, dialog/reconnect subscenarios, render coalescing, native display, and console-clean checks remain intact.
- Validation is green: caco-web-observe 12 tests; caco-web lib 646 tests.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA.
- Files touched:
  - `crates/caco-web/src/bin/caco-web-observe.rs` — dynamic command mock frames/subscenario/assertion.
  - `.cacophony/agent/.../summary/pending/web/dynamic-command-submit-test/` — scenario evidence.
- Behavioural delta: no production runtime change; live browser proof now covers dynamic slash command submission semantics.

## Embedded artefacts

- `web/dynamic-command-submit-test/pico-dynamic-submit-observe.log` — scenario log with dynamic_received prompt frame.
- `web/dynamic-command-submit-test/pico-dynamic-submit-server.log` — dev server log.
- `web/dynamic-command-submit-test/screenshots/*.png` and page snapshots — scenario artifacts.
- `web/validation.txt` — command/results summary.

## Operator-takeaway

caco-web Pico now proves dynamic commands from the agent snapshot are both selectable in the UI and submitted as backend-routable prompt frames.
