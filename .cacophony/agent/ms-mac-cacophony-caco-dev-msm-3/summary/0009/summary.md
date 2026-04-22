# Session summary — bd-a3ea49 precise AGE / WORKER_AGE in `caco bd list`

## Goal

`caco bd list` AGE column collapsed everything between 1 and 7 days to the bucket "Nd ago", so triage controllers couldn't tell a 25-hour-old bead from a 6-day-old one without opening each. Same problem for the WORKER_AGE column. Improve precision while keeping the column compact.

## Bead(s)

- `bd-a3ea49` — caco bd list output truncates AGE column ('1d ago') losing precision; sort by recent-activity not creation.

## Scope decision

Bead suggested four improvements: (1) sort by last-updated-at default, (2) AGE precision, (3) consistent WORKER_AGE, (4) `--sort` flag. I narrowed to **(2) + WORKER_AGE precision (effectively (3))** because:
- (1) requires a daemon-side `last_updated_at` field that doesn't exist yet on the bead JSON contract.
- (4) is small, but only useful after (1).
- (2) + (3) is purely formatter work, single crate, behaviour-preserving.

Filing a follow-up bead to track (1)+(4) is a separate small task for whoever wants the daemon contract change.

## Before state

- `caco_config::human_relative_time_secs(seconds)` collapsed: 60-3599→`Nm ago`, 1-23h→`Nh ago`, 1-6d→`Nd ago`, 7-29d→`Nw ago`, 1-11mo→`Nmo ago`, ≥1y→`Ny ago`.
- `caco bd list` AGE = `human_relative_time(bead.created_at)`. WORKER_AGE = same on `last_claimed_at`.
- A 30-hour-old bead and a 6-day-23-hour-old bead both showed `1d ago`/`6d ago` with no sub-day resolution.

## After state

- New `caco_config::human_relative_time_precise_secs(secs)` and `human_relative_time_precise(timestamp)` keep two units of precision once the duration crosses an hour:
  - `< 60s` → `just now`
  - `60s..1h` → `Nm ago` (sub-minute is just noise)
  - `1h..24h` → `NhMm ago` (e.g. `1h12m ago`, `23h59m ago`)
  - `1d..7d` → `NdMh ago` (e.g. `1d4h ago`, `6d23h ago`)
  - `1w..4w` → `NwDd ago` (e.g. `1w3d ago`)
  - `1mo..12mo` → `NmoDd ago` (e.g. `1mo15d ago`)
  - `≥1y` → `NyMmo ago` (e.g. `1y2mo ago`)
  - When the second unit is zero it's omitted (`1h ago`, not `1h0m ago`).
- `caco bd list` AGE and WORKER_AGE both call the precise variant. Column min_width 10 still fits the longest output (`1mo15d ago` = 10 chars).
- Original `human_relative_time` / `human_relative_time_secs` left untouched — every other call site (bead detail view, agent listings, dispatch metadata) keeps the compact format.

## Folded-in fixes (broken-on-main)

`cargo clippy -p caco-cli --all-targets -- -D warnings` was failing on entry with 4 + 3 lints in `crates/caco-cli/src/disk_breakdown.rs` (recently landed):

- L17,18,26 `clippy::doc_overindented_list_items`: continuation indentation on bullets — reformatted to 2-space body indent.
- L32 `clippy::doc_invalid_quoted_doc_quotes`: `>50%` parsed as a quote line — replaced with `more than 50%`.
- L550-553 `clippy::useless_vec`: `&vec![0u8; N]` for static-size buffers → `&[0u8; N]`.

Folded into this commit (per session convention) so the test-health lane doesn't have to file/claim a separate cycle.

## Diff summary

- Files touched:
  - `crates/caco-config/src/relative_time.rs` (new precise formatter + parser, +84 lines, +2 unit tests covering bucket boundaries and RFC-3339 round-trip).
  - `crates/caco-config/src/lib.rs` (re-export `human_relative_time_precise`).
  - `crates/caco-cli/src/lib.rs` (AGE + WORKER_AGE call sites in `dispatch_bd_list`).
  - `crates/caco-cli/src/disk_breakdown.rs` (broken-on-main clippy fixes).
- Tests: 2 new (`precise_keeps_two_units`, `precise_parse_rfc3339`); existing 12 in this module still pass.
- Clippy: `cargo clippy --workspace --all-targets -- -D warnings` clean.

## Operator-takeaway

After this lands, `caco bd list` AGE will look like `1h12m ago`, `1d4h ago`, `2w3d ago` — same column width, much more useful for triage. If you have any scripts that grep `Nd ago` exactly, they'll need to allow the trailing hour suffix.

Sort-by-last-activity (the bead's "default sort" point) is **not** in this change — needs a daemon-side `last_updated_at` field. Will file a follow-up next cycle.
