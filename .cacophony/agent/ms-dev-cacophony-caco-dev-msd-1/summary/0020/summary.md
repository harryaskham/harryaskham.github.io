# Session summary — bd-681b44 caco msg stats --bucket

## Goal

Land bd-681b44, the bd-665f37 follow-up adding
`--bucket {minute|hour|day}` to `caco msg stats` for
time-series per-bucket aggregation. Useful for sparkline
visualisations and drift detection.

## Bead(s)

- `bd-681b44` — [bd-665f37 follow-up] caco msg stats --bucket.

## Diff summary

**`crates/caco-cli/src/lib.rs`:**

- New `--bucket` arg in `MSG_STATS_ARGS`.
- `dispatch_msg_stats` gains a `bucket: Option<&str>`
  parameter; when supplied, calls `bucket_msg_stats` and
  emits a `buckets` array in the JSON envelope.
- New `parse_bucket_spec(raw)` helper: case-insensitive,
  accepts `minute|min|m`, `hour|hr|h`, `day|d`.
- New `bucket_msg_stats(messages, since, until,
  bucket_secs)` pure helper: partitions messages into
  fixed-size windows aligned to `since`, returns
  one JSON object per non-empty bucket with `{start, end,
  total, by_kind}`.
- Text mode appends a `buckets:` block when present.
- 2 new tests:
  `msg_stats_bucket_arg_exposed_and_parser_maps_spellings`,
  `bucket_msg_stats_partitions_into_chronological_windows`.

## Before state

- `caco msg stats` returned only the aggregate window
  totals — no time-series breakdown.
- Plotting message rate over time required calling
  `msg history --tail N` and post-processing.
- The `out of scope` note in summary 0019 had explicitly
  flagged this as the natural next slice.

## After state

- `caco msg stats --project P --since 24h --bucket hour`
  returns 24 hourly buckets ready for sparkline / drift
  detection.
- JSON envelope addition is backwards-compatible (when
  `--bucket` not supplied, `buckets` is `[]`).
- Pure helper unit-tested with synthetic messages — no
  daemon round-trip in tests.
- `cargo test-small` 56/56 green.
- `cargo test -p caco-cli --lib bucket_msg_stats` and
  `msg_stats_bucket_arg` green.

## Notes / verification

- Buckets are aligned to `since`, not to wall-clock
  hour/day boundaries. Trade-off: simpler implementation
  + predictable bucket count vs. less natural visualisation.
  Operator can pass an aligned `--since` (e.g.
  `2026-04-22T08:00:00Z`) when they need wall-aligned
  buckets.
- Empty buckets are omitted (BTreeMap iter only includes
  populated entries) — keeps payload small for sparse
  windows. Sparkline renderers will need to fill gaps if
  zero-bucket gaps matter; alternative (pre-fill all
  buckets) would inflate large windows pointlessly.

## Out of scope

- Sparkline TUI/web rendering — separate UI bead.
- Daemon-side time-series (Prometheus) — bd-2c7488.
- Wall-clock-aligned buckets (`--align-to-wall`) — file
  follow-up if needed.
- Top-K-per-bucket — would inflate payload; not needed
  for v1.

## Operator-takeaway

`caco msg stats --project P --since 24h --bucket hour`
now returns per-hour message counts in addition to
window aggregate. Pure additive extension — when
`--bucket` is omitted the output is unchanged from
bd-665f37. Three-bead msg-search/stats cluster
(snapshot/history/stats+bucket) is now complete.
