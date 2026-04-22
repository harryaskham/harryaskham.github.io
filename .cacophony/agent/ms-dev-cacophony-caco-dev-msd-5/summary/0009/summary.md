# Session summary — bd-bf1e86 cycle 8: human_age_ago + de-dup

## Goal

De-duplicate the two view-local age formatters that were missing the
months/years tiers added in cycle 4, so git HEAD age and fetch-stale
labels render consistently with timestamp-based staleness elsewhere.

## Bead(s)

- `bd-bf1e86` — Permanent: caco-tui subtle UX polish (cycle 8)

## Before state

- `views/project_overview.rs::format_duration_short(secs)` and
  `views/status.rs::format_head_age(secs)` were two near-identical
  helpers, both topping out at the days tier.
- A 90-day-old git checkout rendered as `90d ago`; a year-old fetch
  as `400d ago`. Cycle 4 had already taught the timestamp-based
  `human_staleness_ago` to use `mo` / `y`, but the seconds-based
  callers never picked it up.

## After state

- Added `views::common::human_age_ago(secs: u64) -> String` that
  builds a synthetic `Utc::now() - secs` timestamp and delegates to
  `human_staleness_ago`.
- Both view-local helpers now reduce to one-line delegations to
  `common::human_age_ago`.
- Locked with `human_age_ago_mirrors_staleness_ago_tiers` covering
  `now`, `30s`, `5m`, `3h`, `2d`, `3w`, `3mo`, `1y` outputs.

## Diff summary

- Commits: `3166bb07`
- Files: `crates/caco-tui/src/views/common.rs`,
  `crates/caco-tui/src/views/project_overview.rs`,
  `crates/caco-tui/src/views/status.rs`
- +40 / -21 lines, +1 test.
- Build + clippy clean on caco-tui.

## Operator-takeaway

Time-since rendering is now sourced from a single helper with
consistent tiers across both timestamp-based and seconds-based
callers. Long-stale checkouts and HEAD ages no longer balloon into
multi-day numbers.
