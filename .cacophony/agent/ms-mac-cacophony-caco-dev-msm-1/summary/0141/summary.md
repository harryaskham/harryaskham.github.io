# Session summary — remove bead CRUD sync preflights

## Goal

Address the P0 operator requirement that public bead CRUD must behave like a normal daemon API and stop leaking git sync/freshness implementation details to agents during routine claim/close/update/create workflows.

## Bead(s)

- `bd-3ae61c` — Simplify beads CRUD API so git sync state never leaks to agents

## Before state

- Failing tests: no code test was failing yet, but live `caco bd claim --bead-id bd-3ae61c` repeatedly refused before reaching the daemon because the CLI performed a board-visibility preflight and returned `board_visibility_*` style guidance tied to `sync_in_progress`, `ahead`, and `behind`.
- Relevant metrics: the obsolete `bd_board_mutation_preflight` / `bd_board_status_allows_mutation` path rejected claim/close whenever `/api/v1/beads/status` reported stale, ahead, behind, syncing, degraded, or checkout-blocked state.
- Context: this was exactly the abstraction leak the operator called out — agents were being taught to poll sync internals instead of trusting CRUD commands.

## After state

- Failing tests: none observed in focused validation.
- Relevant metrics: queued `tj-02a46440` passed `RUST_MIN_STACK=33554432 cargo test -p caco-cli bd_mutating_commands_do_not_expose_board_visibility_preflight_bd_3ae61c --lib -- --test-threads=2`; `git diff --check` passed.
- Context: `caco bd claim` and `caco bd close` now dispatch directly to the authoritative daemon route without the removed client-side sync/freshness preflight. Docs and worker guidance now say sync/freshness errors on public bead mutations are bugs to route/fix under `bd-3ae61c`, not retry choreography to normalize.

## Diff summary

- Commits: `15abdd998`.
- Files touched: `crates/caco-cli/src/lib.rs`, `README.md`, `AGENTS.md`, `SPEC.md`.
- Tests: replaced stale board-visibility preflight tests with a regression that asserts caco-cli no longer contains the public write-surface freshness preflight/error guidance.
- Behavioural delta: routine bead claim/close calls no longer fail locally because the CLI inspected git sync state; diagnostic sync fields remain available through explicit status/doctor surfaces.

## Operator-takeaway

The immediate agent-facing abstraction leak was in the CLI, not agent discipline: removing that preflight makes the public `caco bd` write surface match the intended CRUD contract and updates the profile/docs so future agents fix similar leaks instead of polling around them.
