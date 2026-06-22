# Session summary — bd-5bad1b slice 2c-ii: render app_store_records in caco release list

## Goal

Complete the read/surface side of the Google Play release-tracking feature
(bd-5bad1b) by rendering the stored mobile app-store release records in
`caco release list`, so operators can see Play/WearOS track state (version code,
status, release name) alongside GitHub release jobs.

## Bead(s)

- `bd-5bad1b` — caco release refresh: Google Play tracking for Android +
  WearOS internal tracks (slice 2c-ii of N; bead stays `in_progress`).
  - the whole daemon-side pipeline + the release-list `app_store_records` field
    are already landed; only the live refresh wiring (2b-iii) remains.
- `bd-277119` — (draft) credential-wrapper resolver, gating 2b-iii.

## Before state

- Failing tests: none.
- The daemon list response already carried `app_store_records` (2c-i), but the
  CLI did not render them in text output.

## After state

- Failing tests: none. Focused queued test
  `cargo test -p caco-cli --lib release_cmd::tests::format_app_store_record_line`
  = passed (tj-7fedd3a4, exit 0; the earlier 1200s job timed out on the cold
  caco-cli compile under host saturation — re-run with a 2700s budget passed).
- `caco release list` now prints an "App-store releases" section (one compact
  line per record) when present; `--json` already surfaced the field.

## Diff summary

- Code commits: 2c-ii (this slice); final landed squash SHA from the receipt.
- Files touched: `crates/caco-cli/src/release_cmd.rs` (text rendering in
  `dispatch_release_list_impl` + pure `format_app_store_record_line` + a new
  `#[cfg(test)]` module testing it).
- Tests: +1 (field rendering + missing-field placeholders).
- Behavioural delta: `caco release list` text output gains the app-store section
  when records exist; back-compat (older daemon without the field => no rows).

## Operator-takeaway

The read/surface path for Google Play release tracking is now complete: the
daemon stores and serves the records and `caco release list` renders them. The
only remaining piece is the live refresh wiring (2b-iii) — calling
`fetch_play_track` from `caco release refresh` — which is gated on the
credential-wrapper resolver filed as bd-277119. Once that lands and a live
refresh verifies end-to-end, bd-5bad1b can close.
