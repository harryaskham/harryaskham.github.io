# Session summary — bd-aa0724 slice 3a: degraded pending rows in caco release list

## Goal

Satisfy the bead's "actionable degraded rows, not crashes" acceptance: make
`caco release list` show explicit pending rows for configured app-store sources
that have no store record yet, so operators can see Android/WearOS/iOS/watchOS
tracks that are tracked but have not reported state. CLI side of slice 3.

## Bead(s)

- `bd-aa0724` — caco release list/status/config: surface app-store rows (slice 3a
  of 3; bead stays `in_progress` pending the daemon slice 3b).

## Before state

- Failing tests: none.
- `caco release list` showed only live app-store records (bd-5bad1b); configured
  sources without a record were invisible (no degraded signal).

## After state

- Failing tests: none. `cargo test -p caco-cli --lib release_cmd` = passed
  (tj-a78747bf, exit 0): real compile verified (`Compiling caco-cli v1.2.1270`,
  ~14 min), 3 tests passed incl. the new helper test.
- New pure `build_app_store_status_rows(sources, records)` combines record rows
  (`format_app_store_record_line`) with degraded `pending / no record yet` rows
  for configured sources not present in the records.
  `dispatch_release_list_impl` renders via it. Back-compat: when the daemon does
  not surface `app_stores` (pre-3b), only record rows render.

## Diff summary

- Code commits: bd-aa0724 slice 3a; final landed squash SHA from the receipt.
- Files touched: `crates/caco-cli/src/release_cmd.rs` (`build_app_store_status_rows`
  + list render switch + unit test).
- Tests: +1 (record + pending + all-pending + records-only cases).
- Behavioural delta: `caco release list` shows pending degraded rows once the
  daemon provides configured sources (slice 3b). Secret-free.

## Operator-takeaway

The degraded-row signal is now in place on the CLI: configured-but-no-record
app-store sources will render as explicit pending rows in `caco release list`.
Last slice (3b) wires the daemon to include the project's configured app_stores
sources in the list response so these rows populate, then bd-aa0724 closes.
