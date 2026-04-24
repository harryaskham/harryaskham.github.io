# Session summary — webapp choices fetch limit (bd-355162)

## Goal

Fix operator complaint: webapp could not display / resolve choices
presented 14 days ago. Investigation traced the symptom to silent
truncation at the `/api/v1/choices/list` daemon default — same
root-cause class as bd-0b47a7 (event log silent `--limit`
truncation).

## Bead(s)

- `bd-355162` — Resolve old choices from 14d ago in webapp (P2,
  bug, choices/data-retrieval/historical-data/webapp).

## Before state

`crates/caco-daemon/src/choices.rs::handle_list_choices`:
```rust
let limit = query.limit.unwrap_or(100);
```

Webapp's `loadChoices` (`crates/caco-web/static/app.js`) was
hitting `/api/v1/choices/list` (with optional `?status=…`) but
NEVER passed an explicit `limit`. Daemon defaulted to 100. Once
the operator's project accumulated 100+ newer choices, anything
older was silently dropped from the response — including
operator-pending choices the operator still needed to resolve.

A choice presented 14d ago in an active project is well past
this cutoff (cacophony alone runs 218 closes/24h per bd stats,
so 100 newest choices = ~few hours of activity).

## Fix scope

Smallest correct fix at the bead's scope (webapp): pass an
explicit `limit=1000` so a typical 14d retention window is
covered.

Daemon side `unwrap_or(100)` left unchanged (changing the daemon
default risks unrelated CLI surfaces; that's the broader pattern
fix tracked under follow-up bd-8031d0 below).

## After state

`crates/caco-web/static/app.js::loadChoices` now constructs:

```js
const limitParam = 'limit=1000';
const url = statusParam
    ? `/api/v1/choices/list?status=${encodeURIComponent(statusParam)}&${limitParam}`
    : `/api/v1/choices/list?${limitParam}`;
```

Comment cross-references bd-0b47a7 (the broader truncation-
surfacing pattern this is a one-shot mitigation for).

## Diff summary

- Files touched:
  - `crates/caco-web/static/app.js` (loadChoices URL builder, +12 LOC
    inc. comment)
  - `crates/caco-web/src/tests.rs` (+1 test)
- Tests: +1 / -0
  - `app_js_choices_fetch_passes_explicit_limit_for_retention_window`
    pins that loadChoices' `/api/v1/choices/list` URL builder
    includes `limit=1000` (or higher), with a window-scan around
    the URL literal (regression-pin).
- Test command: `cargo test -p caco-web app_js_choices_fetch` → 1 passed.

## Out-of-scope follow-up (filed, NOT closed by this bead)

- **bd-8031d0** (P3): Apply the full bd-0b47a7 truncation-surfacing
  pattern to `/api/v1/choices/list` so the daemon reports
  `total_matched`, `limit`, `truncated` in the envelope and the
  webapp can render "showing N of M — load more" instead of
  hardcoding `limit=1000`. Then revert the hardcode landed here.

## Operator-takeaway

Webapp Choices view now retrieves up to 1000 choices instead of
the silent 100 cap. 14d-old (and even older, in moderate-volume
projects) choices are now visible and resolvable.

The proper fix (load-more pagination + truncation surfacing,
mirroring the audit log fix from bd-0b47a7) is filed as bd-8031d0
for a future bead.

Honored constraints:
- `cargo test -p caco-web app_js_choices_fetch` only — no workspace test.
- Pre-close audit will run before close.
- Operator close-discipline: out-of-scope follow-up filed as new
  bead with back-references; not silently buried.
- Operator `bd update --status=closed` bypass directive: ACK,
  using only `caco bd close`.

23rd bead closed this session (cumulative). 16th in this turn.
