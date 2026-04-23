# Session summary — caco agent log --since (bd-83a8ed)

## Goal

Add `--since <timestamp|duration>` flag to `caco agent log` so operators
can request time-bounded scrollback ("what did the agent do in the last
10 minutes") instead of line-bounded only.

## Bead(s)

- `bd-83a8ed` — [bd-83a84d follow-up] caco agent log --since TS for timestamp-bounded scrollback

## Before state

- `caco agent log` only had `--tail K`, `--head K`, `--all` (line-bounded)
- No way to filter scrollback by time

## After state

- `--since 10m` / `--since 2026-04-23T01:00:00Z` filters captured pane output
- Applied after mode (tail/head/all) filtering, works for both local and remote agents
- Best-effort: uses leading RFC 3339 timestamps in log lines; continuation lines without timestamps are kept once a passing timestamp is seen
- 6 new passing unit tests

## Diff summary

- Commits: 1 (pending)
- Files touched: `crates/caco-cli/src/lib.rs`
- Tests: +6 new passing
- Behavioural delta: `caco agent log --id X --since 10m` returns only lines from the last 10 minutes

## Operator-takeaway

The --since filter is best-effort because tmux capture-pane has no native
per-line timestamps. It works well for daemon/CLI log lines that start with
RFC 3339 timestamps (the common case for agent narration). Multi-line output
blocks are preserved as long as the leading timestamped line passes the filter.
