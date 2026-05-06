# Session summary — ops watch-only stale activity split

## Goal

Fix `caco ops` reporting after daemon recovery so stale `last_tool_activity` accounting on otherwise running, unassigned agents is distinguished from real worker wedges that need blocker handling.

## Bead(s)

- `bd-eb28a4` — Investigate ops reporting many persistent agents severely wedged after daemon recovery

## Before state

- `caco ops status --project cacophony --json` classified `fleet.agent_health` and `workers.blockers` as `blocked` whenever `potentially_stuck` was non-empty.
- The problematic rows could be running or waiting agents with no canonical bead, no resume blocker, and no health cause — often just stale activity accounting after daemon/feed recovery.
- SPEC context: persistent idle/no-tool-activity reporting is advisory and must not imply destructive/manual recreation without stronger evidence.

## After state

- Agent summary rows now carry `stuck_classification`, `stuck_evidence`, and `stale_activity_accounting_only` fields.
- Running/waiting rows with no bead, no resume blocker, no health cause, and only an empty/advisory no-tool-activity error are classified as `watch`, not `blocked`.
- `caco ops` now excludes watch-only stale-activity rows from `workers.blockers`, reports separate blocking vs watch-only counts, and downgrades `fleet.agent_health` / `workers.blockers` to `watch` when those rows are the only issue.

## Diff summary

- Commits: `7d33297ae` (code change; this summary is in the following summary commit)
- Files touched: `crates/caco-daemon/src/lib.rs`, `crates/caco-cli/src/ops_cmd.rs`
- Tests: +3 targeted tests covering daemon summary classification and CLI ops blocker/finding behavior.
- Validation: `cargo fmt --all -- --check` passed; queued job `tj-3c95ee6d` passed `cargo test -p caco-cli bd_eb28a4 --lib && cargo test -p caco-daemon bd_eb28a4 --lib`.
- Behavioural delta: `potentially_stuck` still surfaces advisory stale activity, but ops no longer treats idle unassigned running/waiting rows as worker blockers unless there is a bead, resume blocker, health cause, unreachable-node blocker, or other stronger wedge evidence.

## Operator-takeaway

After recovery windows, ops should now say “watch” for stale activity-only rows instead of implying broad persistent-agent recreation is needed; real failed, assigned, or health-cause wedges remain blockers.
