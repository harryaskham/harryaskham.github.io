# Session summary — bd-b898b9 ended_at backfill for legacy terminal agents

## Goal

Migrate legacy `agent.json` records on disk so terminal-state agents
created before bd-d438bf land an `ended_at` timestamp, fixing
downstream consumers (TUI sort, telemetry, audit-reintegration) that
still see `null` for these records.

## Bead(s)

- `bd-b898b9` — agent.json: backfill ended_at for legacy terminal records (pre-bd-d438bf)

## Before state

- Failing tests: bd-c19193 (pre-existing, unrelated).
- bd-d438bf landed the `ended_at` stamp at transition time, but only for new transitions. Agents that reached `Completed` / `Failed` / `Stopped` before that change kept `ended_at: null` on disk.
- bd-ff753c added a retention-planner fallback to `created_at`, but other consumers — TUI sort, telemetry, audit-reintegration — still saw the null and could not order or summarise these records correctly.

## After state

- Failing tests: bd-c19193 (unchanged, pre-existing).
- `AgentManager::new` now runs a one-shot backfill at daemon startup:
  - For each scanned `AgentInfo` where `state.is_terminal()` and `ended_at.is_none()`, sets `ended_at = updated_at.unwrap_or(created_at)` and persists the record back via `health::persist_agent_json`.
  - Live (non-terminal) agents pass through untouched even if their on-disk record happens to be missing the field.
  - Persist failures log to daemon stderr but are non-fatal: the in-memory record still carries the backfilled value for the current session, so the runtime view is consistent even when disk persistence loses; subsequent `set_state` calls will re-persist via the normal path.
- The migration is anchored to `AgentManager::new` rather than `scan_agents_dir_filtered` so it runs exactly once per daemon boot. Idempotent on re-runs (the `ended_at.is_some()` early-return short-circuits).

## Diff summary

- Commits: `8f09febf bd-b898b9: backfill ended_at for legacy terminal agent.json records`
- Files touched:
  - `crates/caco-daemon/src/agent/mod.rs` (+~35 lines: new `backfill_terminal_ended_at` helper plus a 5-line call in `AgentManager::new`).
  - `crates/caco-daemon/src/agent/tests.rs` (+~250 lines: 3 new tests, mostly struct-literal boilerplate).
- Tests: +3 / -0 / flipped 0
  - `agent_manager_new_backfills_ended_at_for_legacy_terminal_records` — happy path with `updated_at` populated.
  - `agent_manager_new_backfills_ended_at_falls_back_to_created_at` — fallback when `updated_at` is also missing.
  - `agent_manager_new_does_not_backfill_live_agents` — Running agents are not migrated.
- Behavioural delta: legacy terminal records' `ended_at` becomes non-null on first daemon boot after this lands. No change for modern records or live agents. No new public surface — the helper is private to `agent::mod`.

## Operator-takeaway

This is a passive migration: operators do not need to invoke
anything. The bead suggested an alternative `caco agent fix-metadata`
subcommand, but anchoring the work to startup avoids an
operator-driven step entirely and ensures every daemon node
self-heals its own legacy records on the next boot. If a future
need arises to re-run the backfill for non-startup reasons (e.g.
field added later), the helper `backfill_terminal_ended_at` is
already extracted and ready to be wired into a new entry point.
