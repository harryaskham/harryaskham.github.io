# Session summary — Pi inbox watcher first slice

## Goal

Implement the first focused slice of `bd-ec3e34`: replace noisy prompt-loop inbox checks with a repo-owned Pi-native inbox delivery path that injects only actual new messages.

## Bead(s)

- `bd-ec3e34` — Replace prompt-loop inbox polling with Pi-native inbox event plugin

## Changes

- Added `.cacophony/pi/inbox/extensions/caco-inbox.mjs`:
  - Polls `caco msg inbox --json` from inside the Pi runtime using the active project identity.
  - Tracks delivered message IDs in Pi session custom entries (`caco-inbox-state`).
  - Seeds the current inbox on first run by default so agents do not receive a historical flood.
  - Injects only newly arrived direct/broadcast messages with concise provenance.
  - Avoids self-originated messages and non-matching direct-message targets.
  - Queues delivery as `followUp` while busy by default, with `steer` available via env.
  - Persists recoverable restart-window warnings quietly instead of injecting no-op/error prompts.
  - Exposes `/caco-inbox status`, `/caco-inbox poll`, `/caco-inbox on`, and `/caco-inbox off`.
- Added `.cacophony/pi/inbox/extensions/caco-inbox.test.mjs` with deterministic node:test coverage for:
  - deduplication/filtering/order;
  - prompt formatting/provenance;
  - state restoration;
  - initial seeding followed by new-message injection;
  - quiet restart-window warning persistence.
- Added `.cacophony/profiles/pi-inbox.md` profile mixin and wired `.cacophony/agents/pi-common.yaml` to compose `pi-inbox` before `pi-loop`.
- Updated `SPEC.md`, `README.md`, `AGENTS.md`, `docs/agents.html`, and regenerated `docs/profiles.html` to document the `pi-inbox` runtime path and its relationship to `/loop` / self-nudge.

## Validation

- `node --test .cacophony/pi/inbox/extensions/caco-inbox.test.mjs` — passed.
- `caco config validate --config .cacophony/config.yaml` — passed with existing non-fatal warnings.
- `cargo run -p caco-profile --bin caco-docs-gen -- --check` — passed.
- `docs/validate-pages.sh` — passed.
- `git diff --check` — passed.

## Notes

- This slice implements polling through first-party `caco msg inbox --json`; future slices can replace the polling backend with a daemon push/subscription API if one lands.
- Existing agents will need profile artifact refresh/recreate to pick up the new `pi-inbox` mixin in already-materialized workspaces.
