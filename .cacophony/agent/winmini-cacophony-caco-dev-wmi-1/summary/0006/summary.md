# Session summary — bd-655352 orphaned agent dirs in retention prune

## Goal

Fix the prune blind spot where filesystem-resident agent directories could sit on disk outside the daemon-index view and therefore never appear in `caco agent prune` planning, even during disk-pressure triage.

## Bead(s)

- `bd-655352` — Prune misses orphaned agent directories absent from daemon index

## Before state

- Failing tests: no targeted regression covered this prune-planning blind spot.
- Relevant metrics: `dispatch_agent_prune(...)` built its retention inventory from `AgentManager::new(...).list_all()`, which excludes discarded agents because manager hydration uses the non-discarded scan path. That meant `--include-discarded` could not actually see discarded disk-only agent records in planning. Separately, directories under `~/.cacophony/agents/<project>/<id>/` with missing or broken `agent.json` were invisible to prune accounting entirely.
- Context: during disk-pressure triage, operators saw multi-gigabyte agent directories on disk that were absent from prune target/excluded accounting, making it impossible to tell whether they were safe to reclaim or silently orphaned.

## After state

- Failing tests: none in the focused caco-cli prune lane.
- Relevant metrics: `caco agent prune` now builds retention inventory from `scan_agents_dir_all(...)` so discarded disk records are visible to planning, and it separately scans filesystem-only agent directories not represented in parsed inventory. Missing, unreadable, or unparseable `agent.json` directories are now surfaced in excluded accounting with explicit reasons instead of disappearing from the report.
- Context: this does not silently auto-delete metadata-less directories; it makes them operator-visible with concrete reasons and byte counts so disk-pressure cleanup can proceed intentionally.

## Diff summary

- Commits: `f4bc54997`
- Files touched: `crates/caco-cli/src/audio_cmd.rs`
- Tests: `cargo test -p caco-cli retention_inventory_agents_includes_discarded_bd_655352 -- --nocapture`; `cargo test -p caco-cli scan_retention_orphan_agent_dirs_reports_missing_and_unparseable_bd_655352 -- --nocapture`
- Behavioural delta: `caco agent prune --dry-run` no longer silently omits discarded disk-only records or filesystem-only agent dirs with missing/broken metadata; those paths now show up in excluded accounting with byte counts and reasons.

## Operator-takeaway

This makes disk-pressure triage safer and more trustworthy: prune planning now tells you about orphaned agent directories it cannot yet reclaim automatically, instead of pretending they do not exist.
