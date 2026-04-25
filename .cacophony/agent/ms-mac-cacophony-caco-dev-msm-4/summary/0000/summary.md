# Session summary — bd-9a8433 canonical --since errors

## Goal
Fix cross-surface CLI wording drift so `caco summary`, `caco agent audit-reintegration`, and `caco agent merge-queue list` report empty and invalid `--since` values with the same quoted-value format hint used by timeline/event-log/msg inbox.

## Bead(s)

- `bd-9a8433` — caco summary + audit/merge-queue `--since` phrasing drift

## Before state

- Family-A surfaces emitted terse empty-value errors such as `--since value cannot be empty` with no duration/RFC3339 hint.
- Non-empty invalid values used an older colon format: `invalid --since value: bogus (...)`.
- Family-B surfaces already used `invalid --since value 'bogus' (expected e.g. 3h, 30m, 1d or RFC 3339 timestamp)`.

## After state

- `parse_since_duration` now emits the canonical quoted-value message for empty strings, bad units, and non-numeric values.
- The three repro surfaces are pinned by a regression test for both `--since ''` and `--since bogus`.
- Existing duration/RFC3339 acceptance and negative-duration guidance are preserved.

## Diff summary

- Commit: `dde33c90d` after stale-branch replay.
- Files touched: `crates/caco-cli/src/lib.rs`.
- Tests: added `bd_9a8433_since_empty_and_bogus_use_canonical_format_hint`; tightened `parse_since_duration_rejects_*` assertions.
- Validation: `cargo test -p caco-cli bd_9a8433 --lib`; `cargo test -p caco-cli parse_since_duration_rejects --lib`; `cargo clippy -p caco-cli --all-targets -- -D warnings`; `cargo check --workspace --tests`.

## Operator-takeaway

The CLI now gives the same useful `--since` format hint on the summary and agent audit/merge-queue surfaces as it already did on timeline, event log, and message inbox.
