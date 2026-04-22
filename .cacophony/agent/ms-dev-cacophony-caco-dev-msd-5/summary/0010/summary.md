# Session summary — bd-bf1e86 cycle 9: merge_queue staleness delegation

## Goal

Collapse the third copy of the seconds-tier age formatter
(`merge_queue::relative_age`) to delegate to
`common::human_staleness_ago`, picking up the cycle 4 months/years
tiers and matching the consolidation pattern from cycle 8.

## Bead(s)

- `bd-bf1e86` — Permanent: caco-tui subtle UX polish (cycle 9)

## Before state

- `views/merge_queue.rs::relative_age(ts: &str)` parsed RFC3339,
  computed signed duration, and produced `Ns/Nm/Nh/Nd ago` inline,
  with no months/years tier — so old merge queue entries balloon to
  triple-digit days.

## After state

- `relative_age` is now `parse RFC3339 -> human_staleness_ago`. The
  fallback for unparseable input is preserved (returns the raw
  string).
- All staleness rendering across the TUI now flows through a single
  helper.
- Existing tests use `ends_with("m ago")` / `ends_with("h ago")`
  assertions and remain green.

## Diff summary

- Commits: `67414606`
- Files: `crates/caco-tui/src/views/merge_queue.rs`
- +6 / -17 lines, no new tests (existing tests cover the path).
- Build + clippy clean on caco-tui.

## Operator-takeaway

Merge queue rows now show `3mo ago` / `1y ago` for long-stale
entries, matching beads / inbox / feed / project overview / status.
The seconds-tier age-formatter pattern is now exhausted across
caco-tui — `grep -rE "secs / 86400" crates/caco-tui/src/views` is
empty.
