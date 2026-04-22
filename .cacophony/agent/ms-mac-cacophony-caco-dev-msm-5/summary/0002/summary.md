# Session summary — bd-727210: cap perf_events retention to bound daemon.db

## Goal

Stop helsinki's `daemon.db` from running away (2.2 GB observed, 1.7 GB
of which is `perf_events` table + 5 indexes) and triggering the
`caco doctor` storage check. The existing 7-day retention window is
too generous for a node that ingests ~644k perf events per day.

## Bead(s)

- `bd-727210` — daemon.db perf_events table grew unbounded — 4.5M
  rows / 1.6GB in 7 days

## Before state

- `RETENTION_PERF_EVENTS_DAYS = 7` in
  `crates/caco-daemon/src/store.rs`.
- helsinki: `daemon.db` 2.2 GB; `perf_events` table + indexes ≈ 1.7 GB;
  4.5M rows over 7 days.
- `caco doctor` storage check: error (>500 MB threshold).
- Existing periodic prune loop runs every 15 min and on startup, so
  pruning *is* happening — the window is just too long for the
  ingest rate.

## After state

- `RETENTION_PERF_EVENTS_DAYS = 2` (bd-727210 comment annotates the
  reason inline).
- Doc comment on `prune_retention()` updated to call out the new value
  + bead reference.
- New unit test
  `prune_retention_perf_events_uses_2_day_window` asserts a 3-day-old
  row is dropped and a 1-day-old row is kept (regression: under the old
  7-day window, the 3-day-old row would have been retained).
- Existing prune tests (10-day-old + recent-only) continue to pass.
- Steady-state projection on helsinki: ~1.3M rows × ~370 B avg ≈ ~480
  MB for the full table footprint, comfortably under 500 MB warning.
- `cargo test -p caco-daemon --lib -- prune_retention`: 6/6 pass.

## Diff summary

- Commits: `a683ad9f`
- Files touched: `crates/caco-daemon/src/store.rs` (constant + doc
  comment + 1 new regression test)
- Tests: +1, 0 removed, 0 flipped.
- Behavioural delta: every prune cycle (every 15 min and on daemon
  startup) deletes perf_events rows older than 2 days instead of 7.
  No new code paths, no new tables, no plumbing changes.

## Operator-takeaway

The first prune after this version lands will delete most of the
existing perf_events backlog on busy nodes (helsinki especially);
expect a one-off `caco doctor` notice that disk freed up. If steady-
state size is still creeping up after the next 15-min prune cycle,
the next lever is dropping the indexes that aren't carrying their
weight (sqlite_autoindex_perf_events_1 is 220 MB by itself) — that
is a follow-up bead, not part of this fix.
