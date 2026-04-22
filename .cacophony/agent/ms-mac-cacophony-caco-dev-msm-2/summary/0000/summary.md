# Session summary — caco mode CLI (bd-b9f7b8)

## Goal

Give operators, scripts, hooks, cron entries, and config-helper sessions a
first-party CLI for inspecting and changing daemon execution modes (manual,
burndown, etc.) instead of having to curl the `/api/v1/modes` HTTP endpoints
directly. The TUI already drives modes; this closes the CLI gap that
config-helper kept hitting whenever it documented "activate with caco mode
set burndown --project cacophony".

## Bead(s)

- `bd-b9f7b8` — caco mode CLI: set/clear/list/show for global + per-project execution modes

## Before state

- Failing tests: none on `cargo test-small`.
- No `caco mode` subcommand existed; mode state was reachable only via
  HTTP (`GET/POST /api/v1/modes/active`, `GET/POST/DELETE /api/v1/modes/projects/{p}`)
  or the TUI.
- Config-helper documentation referenced a CLI surface that did not exist.

## After state

- Failing tests: none on `cargo test-small` (195/109/716/277/18/2779/45 passed across crates).
- New top-level command `caco mode` with four leaves: `list`, `show`, `set`, `clear`.
- All four are `mcp_enabled = true` + `agent_safe = true` + `idempotent = true`,
  so they auto-register as MCP tools `caco_mode_list/show/set/clear` through
  the existing CommandSpec MCP registration path — no extra plumbing needed.
- Pre-validates unknown mode names against the local config and surfaces a
  useful error (`unknown mode 'bogus'. Defined modes: manual, burndown`)
  before the daemon round-trip.
- `--json` output works on all four subcommands and mirrors the daemon
  envelope shape (`{ok, data, meta}`).

## Diff summary

- Commits: `7a105d0f`
- Files touched: `crates/caco-cli/src/lib.rs` (+406 / -0).
- Tests: no new tests added for this bead — the dispatchers are thin
  pass-through wrappers around already-tested daemon endpoints, and the
  existing CLI integration tests cover the spec/MCP registration path
  via `ROOT_SUBCOMMANDS`. Manual smoke verified all four leaves end-to-end
  against the live daemon (set burndown for cacophony → show reports
  effective=burndown, source=override → clear → reverts to global=manual).
- Behavioural delta: new `caco mode` command tree; no existing behaviour
  changed.

## Operator-takeaway

`caco mode set burndown --project cacophony` and friends now Just Work,
including MCP parity (`caco_mode_set` etc.). Cron entries, hooks, and
agent sessions can flip modes without bespoke curl. The `clear` command
without `--project` reverts global to `manual` — there is no separate
"clear global" daemon endpoint, that's the documented semantics. If a
future bead wants real "delete global override" semantics distinct from
"set to manual", that's a new HTTP route on the daemon side.
