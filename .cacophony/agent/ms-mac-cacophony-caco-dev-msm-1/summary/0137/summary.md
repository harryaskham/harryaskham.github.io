# Session summary — stop infinite summaries background loading

## Goal

Fix the summaries page so it stops background pagination once the backend has no more rows, instead of continuing to request chunks indefinitely and degrading browser performance.

## Bead(s)

- `bd-204efb` — Fix infinite loading of summaries on summaries page

## Before state

- Failing tests: none known for this newly filed web bug, but the operator-visible behaviour was repeated summaries API requests in chunks of 10 even after all available summaries were loaded.
- Relevant metrics: summaries list state tracked `items`, `total`, and `offset`, but did not retain an explicit end-of-list flag and always scheduled another background fetch when there was no background error.
- Context: if an append request returned no usable rows, or fewer than a full page while `total` still suggested more records, the frontend could leave pagination eligible and continue making unnecessary requests.

## After state

- Failing tests: none observed in focused validation.
- Relevant metrics: final `node --check crates/caco-web/static/summaries.js` passed; final `git diff --check` passed; queued `tj-b1c73a8d` passed `cargo test -p caco-web summaries_background_loads_remaining_pages_bd_2dd008 -- --test-threads=2` after retrying one daemon-restart infrastructure outcome (`tj-065a7932`).
- Context: the summaries page now tracks `STATE.hasMore`, consumes the response offset, marks pagination complete when an append makes no progress or returns a short page, hides/blocks manual load-more at completion, and only schedules the next background load when more data is known to be available.

## Diff summary

- Commits: `999ebeacf`.
- Files touched: `crates/caco-web/static/summaries.js`, `crates/caco-web/src/tests.rs`.
- Tests: updated the existing caco-web summaries background pagination regression to assert the new `hasMore` and no-progress guards.
- Behavioural delta: summaries background loading now stops at end-of-list and avoids repeated unnecessary `/api/v1/summaries` calls.

## Operator-takeaway

The summaries page should no longer chew CPU/network forever after it reaches the end of the available summary history; it now treats no-progress/short-page responses as completion.
