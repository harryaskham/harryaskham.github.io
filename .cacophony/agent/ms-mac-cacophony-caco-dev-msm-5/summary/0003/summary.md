# Session summary — timeline view for web (bd-fc8947)

## Goal

Quick-filed bead from operator burndown: ship a per-project +
cluster-wide timeline view in the caco-web surface. Visual SVG
timeline backed by the existing /api/v1/events endpoint, with
filtering by time-window and event-type and a path to deeper
features in sibling beads.

## Bead(s)

- `bd-fc8947` — Create timeline view for web application
- noted duplicate: `bd-9eefcc` (filed 30s after; same scope)
- siblings in lane: bd-f45663 (data service), bd-22cf2c (caching),
  bd-734772 (pipeline), bd-db60a1 (UI component design)

## Before state

- Failing tests: none
- No timeline view; events were only visible via the Feed view as
  a scrolling list, no time-axis visualization.

## After state

- Failing tests: none. `cargo test -p caco-web --lib` = 151 passed
  (+5 new). `cargo clippy -p caco-web --tests` clean (only the two
  pre-existing warnings unrelated to this change).
- Deep-linkable as `#timeline`. Reachable via nav click, kbd 't',
  or hash routing.
- SVG re-renders on resize so it stays sharp on window changes.

## Diff summary

- New: `crates/caco-web/static/timeline.js` (~12kB, self-contained IIFE)
- New: `crates/caco-web/static/timeline.css` (~2kB, scoped)
- Modified: `crates/caco-web/static/index.html` (+nav, +view, +link, +script)
- Modified: `crates/caco-web/static/app.js` (VALID_VIEWS, viewKeys,
  switchView dispatch)
- Modified: `crates/caco-web/src/tests.rs` (+5 tests)

## Operator-takeaway

Timeline is read-only over the existing /api/v1/events endpoint —
no daemon-side changes needed. The data-service / caching siblings
(bd-f45663, bd-22cf2c, bd-734772) can layer on top later without
needing UI changes if they keep the current event-shape contract:
`{ts, command, caller, ...}`.

The classify/timestamp helpers are exposed on `window.Timeline` so
follow-on tests and downstream surfaces (Android web, etc) can reuse
the bucketing rules instead of duplicating them.

bd-9eefcc looks like a duplicate of this bead — flagged in commit
message and to caco-ctrl for dedup on close.
