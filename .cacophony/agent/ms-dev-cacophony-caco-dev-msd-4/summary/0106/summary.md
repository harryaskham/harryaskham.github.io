# Session summary — daemon feed retention bloat recurrence

## Goal

Diagnose and fix the recurring helsinki storage doctor warning for `daemon.db` and `feed.jsonl` after the previous retention and VACUUM work had already landed. The aim was to identify whether the size was dead SQLite space, a still-unbounded table/log, or simply an over-large live retention window, then tighten the implementation enough for busy nodes to fall back below doctor thresholds after the next deployed restart/prune cycle.

## Bead(s)

- `bd-7ce681` — `[doctor] Helsinki daemon.db/feed.jsonl bloat recurred above doctor thresholds after restart`

## Before state

- Failing tests: none known for this bead. An unrelated caco-tui clippy failure was reported by another worker and tracked outside this lane.
- Relevant metrics: helsinki was running `caco 1.2.575`; `daemon.db` was 509 MiB, `feed.jsonl` was about 140 MiB, and `undelivered.jsonl` was about 56 MiB.
- Context: Remote SQLite inspection showed `PRAGMA freelist_count = 0`, so restart/VACUUM could not materially shrink the current file. The largest live footprints were `perf_events` (~170 MiB table plus indexes) and `feed_events` (~146 MiB table plus indexes). `feed.jsonl` retained about 222k lines over a full 14-day window; roughly 60 MiB / 112k lines were older than 7 days, leaving an estimated 86 MiB if pruned to a 7-day active-feed window.

## After state

- Failing tests: none in the touched targeted tests.
- Relevant metrics: Source now retains `feed_events` and `feed.jsonl` for 7 days instead of 14 days. At the observed helsinki distribution this should bring `feed.jsonl` below the 100 MiB doctor warning and reduce `daemon.db` by roughly the old half of the feed index once pruning and VACUUM run on a fixed deployed build.
- Context: `SPEC.md` now explicitly says raw feed storage is a 7-day active ledger and that longer-lived operator history must live in dedicated stores/full-state read models rather than unbounded raw feed replay.

## Diff summary

- Commits: `287ff5d46` (`bd-7ce681: tighten feed retention for daemon storage`) plus the summary-only commit for this record.
- Files touched: `crates/caco-daemon/src/store.rs`, `SPEC.md`.
- Tests: added one regression test for the 7-day `feed_events` retention window and updated the JSONL pruning fixture to exercise 8-day-old vs 6-day-old lines.
- Behavioural delta: daemon retention pruning now deletes raw feed rows and feed log entries older than 7 days. Existing 2-day perf retention and 7-day undelivered hard-cap behavior are unchanged.
- Validation: `cargo fmt --all -- --check`; `cargo test -p caco-daemon prune_retention`; `cargo test -p caco-daemon prune_feed_jsonl`.

## Operator-takeaway

The recurrence was not dead pages waiting for another restart: helsinki had a compact but still-too-large live raw-feed retention window. Seven-day active feed retention is enough for recent operator context and should pull both `daemon.db` and `feed.jsonl` back under the current doctor thresholds once this build is deployed and pruning/VACUUM run.
