# Session summary — Pi explicit-session resume without managed --name

## Goal

Fix the session-selection hazard where managed Pi resume could carry both an explicit session path and the managed display-name flag, causing Pi/pi-home session lookup to risk resuming the wrong project when names collide or when pi-home override changes the working/session context.

## Bead(s)

- `bd-adbe30` — Investigate agents resuming in wrong projects with --name for named sessions
- `bd-4948a3` — Prefer pi resume --session-id over --name for resuming sessions
- `bd-143dbf` — Add session naming on first load when no session ID exists

## Before state

- Failing tests: current-main caco-daemon lib tests initially did not compile because unrelated test fixtures lagged newly-added `Config.client_nodes` / `ProjectAgentsConfig.groups`; after rebasing to current main, focused session tests could run.
- Relevant metrics: Pi fresh/no-session flows and explicit-session resume flows both inserted the managed `--name ${CACO_AGENT_ID}` flag. Explicit `--session <path>` resume already preferred a concrete session path when available, but still also passed `--name`, leaving a name-based selector in the command line.
- Context: `--name` is useful for first-load/fresh sessions so Pi selectors show the Cacophony agent ID, but it should not participate when a concrete provider session path is available.

## After state

- Failing tests: none in focused validation.
- Relevant metrics:
  - `cargo check -p caco-daemon --lib` passed.
  - `cargo test -p caco-daemon --lib inject_pi_session_flag_strips_prior_managed_name -- --test-threads=1` passed.
  - `cargo test -p caco-daemon --lib pi_session -- --test-threads=1` passed (22 tests).
  - `cargo clippy -p caco-daemon --lib -- -D warnings` passed.
- Context: `inject_pi_session_flag` now strips prior managed `--name` flags and returns an explicit `--session <path>` resume command without re-adding `--name`. No-session `--continue` and fresh launch paths still add `--name` so first-load sessions are named with the stable agent ID.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA.
- Files touched: `crates/caco-daemon/src/agent/spawn.rs`, `crates/caco-daemon/src/agent/tests.rs`, `SPEC.md`, `README.md`, `AGENTS.md`.
- Tests: added/updated Pi session resume tests to assert explicit-session resumes omit `--name`, stale managed names are stripped, and no-session fallback still names the first managed session.
- Behavioural delta: exact Pi session resumes are driven solely by the concrete `--session <path>` handle; display-name-based selection is reserved for fresh/no-session starts.

## Operator-takeaway

For Pi-managed agents, `--name` is now a first-load/no-session naming aid, not part of exact session resume. When Cacophony has a concrete Pi session path, it resumes with `--session <path>` alone, avoiding the wrong-project/name-selection trap while preserving stable Cacophony names for new sessions.
