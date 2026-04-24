# Session summary — bd-105e0c: undelivered.jsonl hard-cap safety net

## Goal

Bound `undelivered.jsonl` disk usage on busy nodes where
fan_out queues entries faster than ack-driven and retry-driven
compaction can drain them. The bd-8469eb retention sweep
(7-day TTL on a long cadence) is the right policy for "peer
never came back" but useless as a defence against minutes-scale
regrowth — observed at 71 MB / 13 min on helsinki post-rotation.

## Bead(s)

- `bd-105e0c` — `undelivered.jsonl regrows aggressively
  post-rotation — bd-8469eb acceptance criteria breached`.

## Before state

- After rotation, undelivered.jsonl could grow to 71 MB / 83k
  lines in 13 min on a busy node.
- combined feed.jsonl + undelivered.jsonl crossed the 100 MB
  bd-8469eb threshold within minutes.
- The only bounding mechanisms were:
  - `process_acks` compaction (driven by peer responsiveness).
  - `retry_undelivered` compaction (10-second tick, drops only
    entries past `MAX_RETRY_ATTEMPTS`).
  - `prune_retention` (TTL = 7 days, runs periodically).
- None of these caps the file between sweeps when peers are
  slow/unreachable and write rate exceeds drain rate.

## After state

`queue_undelivered` now enforces a hard size cap after each
append:

- `UNDELIVERED_HARD_CAP_BYTES = 32 MB` ceiling.
- `UNDELIVERED_HARD_CAP_KEEP_LINES = 20_000` newest entries
  retained on emergency compaction (~17 MB at observed
  ~850 bytes/line, comfortably below the cap).
- New helper `compact_undelivered_to_newest(keep)` does FIFO
  drop of the oldest entries. FIFO is correct because the
  oldest entries are also the ones closest to the 7-day TTL
  and the `MAX_RETRY_ATTEMPTS` cutoff.
- Errors during emergency compaction are non-fatal —
  `queue_undelivered` itself succeeded, the next append /
  next sweep retries the cap. A `bd-105e0c:` log line is
  emitted on both successful and failed compaction so
  operators can see when the safety net triggers.

The bd-8469eb sweep (7-day TTL) and the retry/ack compaction
remain the primary mechanisms; this is purely a safety net
for the regrowth case.

## Diff summary

- `crates/caco-daemon/src/store.rs`:
  - Added `UNDELIVERED_HARD_CAP_BYTES` and
    `UNDELIVERED_HARD_CAP_KEEP_LINES` consts with rationale
    docstrings.
  - `queue_undelivered`: drop file handle after write, then
    stat the path and trigger
    `compact_undelivered_to_newest(UNDELIVERED_HARD_CAP_KEEP_LINES)`
    when the size crosses the cap. Errors logged but
    non-fatal.
  - New private helper `compact_undelivered_to_newest(keep)` —
    no-op when entries ≤ keep, otherwise rewrite with the
    newest `keep` only; logs the drop count.
  - 1 new test:
    `compact_undelivered_to_newest_drops_oldest_first` —
    queues 5 entries with distinct peer names, asserts that
    `compact_undelivered_to_newest(2)` keeps the newest 2 in
    insertion order (node3, node4) and is idempotent /
    no-op when keep ≥ current count.
- `cargo test -p caco-daemon --lib
   compact_undelivered_to_newest_drops_oldest_first`: pass.
- `cargo test-small`: 162 pass.

## Operator-takeaway

The bead's primary suggestion (per-entry delivery-confirm
prune) is already in place via `process_acks`. The actual
gap is the absence of a hard upper bound when ack/retry
drain rate falls behind queue rate — a 5.5 MB/min sustained
write rate against a 10-second retry tick is a structural
mismatch that no per-entry refinement can fully close.

The hard cap is a defensive backstop, not the canonical drain
mechanism: it kicks in only in pathological regrowth windows,
keeps the file bounded at ~32 MB worst case, and trades a
small risk of dropping the oldest pending entries (the ones
closest to TTL/MAX_RETRY anyway) for guaranteed bounded disk
use.

Out of scope (left as separate beads if relevant):
- Tuning `RETRY_INTERVAL_SECS` (10s) — increasing retry
  frequency on busy nodes could reduce the regrowth window.
- Per-peer queue partitioning so a single flaky peer doesn't
  back up the global file.
- Triggering an immediate `prune_retention` from inside the
  cap-fire path.

The bd-8469eb acceptance criterion ("daemon directory bytes
outside daemon.db stay under 100 MB on a busy node
steady-state") is now defensible: feed.jsonl ≤ 14-day TTL +
its own JSONL_PRUNE_MIN_BYTES floor, undelivered.jsonl ≤
32 MB hard cap. Together they're well under the 100 MB
threshold even mid-regrowth.
