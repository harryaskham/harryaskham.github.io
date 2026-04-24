# Session summary — Truncation-surfacing for choices list

## Goal

Close bd-8031d0: apply the bd-0b47a7 truncation-surfacing pattern to
`/api/v1/choices/list` so operators see when the result was capped by
`--limit` instead of silently missing older choices.

## Bead(s)

- `bd-8031d0` — Apply bd-0b47a7 truncation-surfacing pattern to
  /api/v1/choices/list (total_matched + truncated + limit fields)

## Before state

- Daemon returned `{choices, count, status_filter}` with no indication
  of whether the limit truncated the results.
- Webapp hardcoded `limit=1000` (bd-355162) as a workaround.
- CLI showed `N choice(s)` with no truncation awareness.

## After state

- Daemon returns `{choices, count, total_matched, limit, truncated, status_filter}`.
- New `count_items()` SQL helper and `query_choices_by_status_with_total()`.
- CLI prints "showing N of M total choice(s) (cap: --limit X) — increase
  --limit to see more" when truncated.
- Webapp captures truncation metadata and shows tooltip on count element.
- 1 new unit test for the `_with_total` function (limit < total, limit >=
  total, status-filtered).

## Diff summary

- Files: `crates/caco-daemon/src/operator_inbox.rs` (+103),
  `crates/caco-daemon/src/choices.rs` (+34/-1),
  `crates/caco-cli/src/lib.rs` (+12),
  `crates/caco-web/static/app.js` (+39/-9)
- Tests: 183 test-small pass. 1 new unit test.

## Operator-takeaway

`caco choices list` and the webapp choices tab now tell you when the
result was truncated by the limit cap. If you see "showing 100 of 250
total choice(s) (cap: --limit 100)", pass `--limit 250` to see all.
The webapp keeps limit=1000 for now but shows a tooltip when truncated.
