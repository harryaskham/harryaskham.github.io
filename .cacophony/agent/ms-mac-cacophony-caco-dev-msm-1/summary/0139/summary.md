# Session summary — Pi Cacophony slash-command widgets

## Goal

Add a first-party managed Pi extension that exposes useful Cacophony command families as discoverable slash commands inside Pi sessions, while preserving canonical `caco` CLI execution and rendering structured results as compact widgets instead of raw JSON dumps.

## Bead(s)

- `bd-0b8001` — Add first-party Pi slash commands for caco command families with rich widget output

## Before state

- Failing tests: none known for this feature bead.
- Relevant metrics: managed Pi profiles already composed repo-owned inbox, loop, self-compact, self-nudge, self-ops, image guard, sudo runner, Tendril, and git-check overlays, but there was no repo-owned `/caco-*` command suite for common Cacophony command families.
- Context: agents and operators had to type raw `caco ...` commands manually from shells or rely on tool calls, losing Pi-native command discoverability and compact widget rendering.

## After state

- Failing tests: none observed in focused validation.
- Relevant metrics: `node --test .cacophony/pi/caco-commands/extensions/caco-commands.test.mjs` passed; `caco config validate --strict --project-config-dir .cacophony` passed; `git diff --check` passed.
- Context: the new `pi-caco-commands` mixin is in the default Pi common stack and materializes `.cacophony/pi/caco-commands/`, which registers `/caco-help`, agent, bead, message, choices, test, and build inspection commands.

## Diff summary

- Commits: `6f36b9fcf`.
- Files touched: `.cacophony/pi/caco-commands/extensions/caco-commands.mjs`, `.cacophony/pi/caco-commands/extensions/caco-commands.test.mjs`, `.cacophony/profiles/pi-caco-commands.md`, `.cacophony/agents/pi-common.yaml`, `README.md`, `AGENTS.md`, `SPEC.md`.
- Tests: added extension-level Node smoke tests covering invocation building, help output, agent status, bead list/show, message inbox, and choices widget rendering.
- Behavioural delta: managed Pi workspaces now get discoverable `/caco-*` slash commands that execute canonical `caco ... --json` surfaces with normal auth/project/agent resolution and render compact below-editor widgets.

## Operator-takeaway

Pi sessions now have a first-party, profile-installed Cacophony command palette for common inspection workflows; it is intentionally a widget-rendering wrapper around canonical `caco` CLI surfaces, not a parallel control protocol.
