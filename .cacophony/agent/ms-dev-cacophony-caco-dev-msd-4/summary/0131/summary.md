# Session summary — bounded project agent list

## Goal

Fix the follow-up to `bd-6d53ae`: `caco agent list --project collective --json` could hang for Collective even while msg/status/bd surfaces were responsive. The goal was to keep the canonical agent-list command bounded and project-scoped without triggering recovery from the Collective controller.

## Bead(s)

- `bd-4cba06` — caco agent list can hang for Collective while msg/status/bd surfaces stay healthy

## Before state

- Collective reported repeated `caco agent list --project collective --json` hangs/timeouts under 20s, 30s, and 120s wrappers.
- Other first-party surfaces in the same windows succeeded, including inbox, fleet summary, self status, and bead list checks.
- Investigation found the CLI still called the all-project `/api/v1/agents` aggregate even when `--project <name>` was set, then filtered client-side. That made a project-scoped Collective query wait behind unrelated all-project agent inventory work.

## After state

- `caco agent list --project <project>` now calls `/api/v1/projects/<project>/agents` directly.
- The CLI daemon read for agent list is bounded to one 8s request; on failure/timeout it returns a degraded disk fallback instead of hanging, with JSON fields `degraded`, `source`, `timeout_secs`, `daemon_error`, and `recovery`.
- The daemon project endpoint now uses `AgentManager::list_with_disk_refresh(project)` and `scan_agents_project_dir`, so project-scoped listing refreshes only `agents/<project>/` instead of scanning every project and filtering afterward.

## Diff summary

- Commits: agent-branch code commit `22a8a5c37` plus this summary commit; final mainline squash SHA is assigned during reintegration.
- Files touched: `crates/caco-cli/src/lib.rs`, `crates/caco-daemon/src/agent/lifecycle.rs`, `crates/caco-daemon/src/agent/mod.rs`, `crates/caco-daemon/src/agent/tests.rs`, `crates/caco-daemon/src/lib.rs`, `.cacophony/agent/ms-dev-cacophony-caco-dev-msd-4/summary/pending/summary.md`
- Tests: +2 caco-cli unit tests for project endpoint URL selection and project-endpoint response parsing; +1 caco-daemon unit test for project-only agent directory scanning.
- Validation: `cargo fmt --all -- --check` passed; queued `tj-5faa111c` passed `cargo test -p caco-cli bd_4cba06 --lib && cargo test -p caco-daemon bd_4cba06 --lib`; queued `bj-5206127e` passed `cargo build -p caco`; queued `tj-d1c5d40d` proved the rebuilt binary returned `ok=true` for `agent list --project collective --json` inside a 25s bound with 4 agents; post-rebase queued `tj-8d3a593f` passed the focused CLI+daemon bd_4cba06 tests again.
- Behavioural delta: a slow all-project inventory path no longer blocks a project-scoped agent list, and daemon/fallback failures now surface bounded degraded diagnostics rather than silence.

## Operator-takeaway

The hang was caused by project-scoped CLI intent being routed through an all-project aggregate. The command is now scoped end-to-end and has an explicit timeout/fallback path, so Collective controllers should get either current project agents or clear degraded JSON within the bounded probe window.
