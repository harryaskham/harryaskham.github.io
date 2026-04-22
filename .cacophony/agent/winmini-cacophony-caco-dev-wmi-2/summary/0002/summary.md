# Session summary — images table retention pruning (bd-713cd8)

## Goal

The bd-20381a audit flagged the daemon's `images` table as the
highest-disk-pressure unmanaged surface in the SQLite store: PNG blobs
(often hundreds of KB to MB each) accumulated forever with no
time-based retention. Land a safe, conservative pruner that mirrors the
existing retention pattern in `store::prune_retention` and is paired
with a feed-event liveness check so live conversations are not damaged.

## Bead(s)

- `bd-713cd8` — [bd-20381a child] images table has no retention pruning
  (parent: `bd-20381a` retention audit; sibling: `bd-a0a229`
  note_delivery_tracking pruning, still open)

## Before state

- `images` table: only DELETE path was the explicit
  `DELETE /api/v1/images/{id}` handler. `store::prune_retention`
  ignored the table entirely.
- `feed_events.payload` JSON embeds an `image_id` field for every image
  upload, but there is no FK column linking the two tables, so a
  liveness check has to run against payload text.
- `cargo test-small`: 4189 tests passing.

## After state

- New `crates/caco-daemon/src/images.rs::prune_unreferenced(db, days)`
  returning `(rows_deleted, bytes_reclaimed)`. Only deletes images
  older than the cutoff that have no surviving `feed_events` row whose
  `payload` matches an anchored
  `LIKE '%"image_id":"<id>"%'` needle. Bytes reclaimed are decoded
  image sizes (the column inflates ~33% with base64).
- `crates/caco-daemon/src/store.rs`: new
  `RETENTION_IMAGES_DAYS = 30` constant; `prune_retention` now
  invokes `images::prune_unreferenced` after the existing
  notifications + exceptions prunes (failures logged, non-fatal,
  matching the JSONL prune pattern). `PruneStats` gains `images` +
  `images_bytes` fields, included in `total()` and the `Display`
  output. Test fixture (`prune_stats_display`) updated.
- `cargo test-small`: 4198 passing (+9: 5 new image-prune unit tests
  + 4 nav delta from the merged conflict resolution).
- `cargo clippy --workspace --all-targets -- -D warnings`: PASS clean.

## Diff summary

- Commit (this session, post-rebase): `ce91401b`
- Files touched (2, +229 / -1):
  - `crates/caco-daemon/src/images.rs` — new
    `prune_unreferenced` + 5 unit tests
  - `crates/caco-daemon/src/store.rs` — RETENTION_IMAGES_DAYS const,
    prune_retention call site, PruneStats fields + Display + total()
    + test fixture (rebase merge with bd-8bb05a notifications work
    that landed concurrently)
- Tests: +5 new (`prune_unreferenced_*`) / 0 removed / 0 flipped.
- Behavioural delta: daemon retention pass now removes old
  unreferenced images during normal `prune_retention` runs (called on
  startup and via `caco db prune`). Live images stay alive as long as
  their referencing feed_events row does, regardless of age.

## Operator-takeaway

The retention audit (bd-20381a) parented several of these "table X has
no time bound" follow-ups. This one was the highest-impact: 30d +
liveness, fail-safe by default. The remaining child (bd-a0a229,
`note_delivery_tracking`) is similar in shape but lower-impact (small
bookkeeping rows, not multi-MB blobs). When that lands the per-table
retention coverage gap from bd-20381a should be closed.
