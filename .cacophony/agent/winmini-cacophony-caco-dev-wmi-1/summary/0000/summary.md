# Session summary — Cluster pulse hero on caco-web homepage

## Goal

Replace the static status-hero copy block on the caco-web dashboard homepage
with a live, theme-matched, animated cluster graph that visually conveys
fleet activity. The operator should be able to look at the homepage and
literally see nodes talking to each other — broadcasts ripple, DMs travel
along the sender→target edge, and the whole graph oscillates gently.

## Bead(s)

- `bd-f4ac16` — cluster pulse hero on web TUI homepage should have beautiful,
  pulsing, theme matched live graph showing cluster
- (related: `bd-1c0bdd` — Permanent Android + caco-web unified UX polish)

## Before state

- Failing tests: none in `caco-web` (45 passed)
- `.status-hero` was a flat 2-column grid: copy on the left, four pills on
  the right. No live cluster visualization anywhere on the homepage.
- Cluster topology was only visible in the Nodes tab as static cards.

## After state

- Failing tests: none in `caco-web` (48 passed; +3 new)
- `.status-hero` now has a `<canvas id="cluster-pulse-canvas">` rendering an
  animated cluster graph behind the existing copy/meta pills (z-indexed).
- Each configured node is a Nord-palette glowing circle on a slowly
  oscillating ellipse; agents orbit their host as small satellites with
  brightness keyed off `state === 'running'`.
- Live SSE feed events emit pulse particles that travel along inter-node
  quadratic curves: targeted DMs follow `sender→target`, broadcasts /
  speaks / chat ripple to all peer nodes from `sender`'s host.
- Animation respects `prefers-reduced-motion` (12 FPS cap, lower opacity)
  and pauses when the tab is hidden.

## Diff summary

- Commits: `56eb0b11`
- Files touched:
  - `crates/caco-web/static/index.html` (+3 lines: canvas + overlay layers)
  - `crates/caco-web/static/style.css` (+33 lines: canvas/overlay styles,
    reduced-motion handling, z-index lift for hero text)
  - `crates/caco-web/static/app.js` (+~290 lines: self-contained
    `clusterPulse` IIFE module, `applySnapshot`/`handleFeedEvent` non-
    destructive function wrapping)
  - `crates/caco-web/src/tests.rs` (+3 tests: canvas presence, JS module
    presence + hook surface, CSS rules presence)
- Tests: +3 / -0 / flipped 0 (caco-web lib: 45 → 48 passing)
- Behavioural delta: visible animated graph on the homepage that updates
  in real time from the existing SSE feed stream; no new endpoints, no
  daemon-side changes, no schema changes.

## Embedded artefacts

(none — pure static-asset change; visual will land via the next caco-web
deploy)

## Operator-takeaway

The homepage is now a live cluster monitor at a glance. All wiring uses
the existing `applySnapshot` and `handleFeedEvent` paths — no new APIs,
no new dependencies, ~290 lines of vanilla canvas. The module is fully
self-contained behind one IIFE and degrades to a static gradient if the
canvas context can't initialise, so it can't break the rest of the
dashboard. Future iterations could add hover tooltips on agent
satellites, click-to-navigate-to-agent, and per-edge throughput meters.
