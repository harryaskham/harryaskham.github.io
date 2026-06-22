# Session summary — bd-5bad1b slice 2c-i: app_store_records in release list response

## Goal

Continue the Google Play release-tracking feature (bd-5bad1b) by surfacing the
stored mobile app-store release records through the existing release-list
daemon endpoint, additively and back-compatibly, so `caco release list` (and
downstream surfaces) can show Play/WearOS track state alongside GitHub release
jobs.

## Bead(s)

- `bd-5bad1b` — caco release refresh: Google Play tracking for Android +
  WearOS internal tracks (slice 2c-i of N; bead stays `in_progress`).
  - prior landed: the full daemon-side Play fetch (parser, store, orchestration,
    helpers, signer, `fetch_play_track`).
- `bd-277119` — (draft) credential-wrapper resolver, gating the live refresh
  wiring (2b-iii).

## Before state

- Failing tests: none.
- `GET /api/v1/projects/{project}/releases` (`handle_release_list`) returned
  only the release-jobs array (`SuccessEnvelope::new(jobs, ...)`); the persisted
  `app_store_records` were never exposed.

## After state

- Failing tests: none. Focused queued test
  `cargo test -p caco-daemon --lib release_play` = passed (tj-eb100578, exit 0).
- The list response now additively includes a top-level `app_store_records`
  array of `{source_id, ...record}` objects (only when non-empty); the `data`
  jobs array is unchanged, so older clients ignore the new field.

## Diff summary

- Code commits: 2c-i (this slice); final landed squash SHA from the receipt.
- Files touched: `crates/caco-daemon/src/release_play.rs` (pure
  `merge_app_store_records_into_release_list` helper + test);
  `crates/caco-daemon/src/lib.rs` (`handle_release_list` calls it with
  `app_store_records_for_project`).
- Tests: +1 (merge is additive, empty-records leaves the response unchanged,
  records add `app_store_records` without touching `data`).
- Behavioural delta: `caco release list --json` will include `app_store_records`
  once any are stored; CLI rendering of those rows lands in 2c-ii.

## Operator-takeaway

The release-list endpoint now carries the stored Play release records
back-compatibly (additive top-level field, `data` untouched). The remaining
work is the CLI rendering of those rows (2c-ii) and the live refresh wiring
(2b-iii), which is gated on the credential-wrapper resolver filed as bd-277119.
The whole bd-5bad1b feature was built emulator-free because the Android emulator
QA path was un-runnable under the fleet-restart load thrashing.
