# Session summary — bd-34a693: filter-chip overflow affordance on narrow viewports

## Goal

Restart the persistent caco-web improvement loop after a recreate: run a
comprehensive Playwright observation pass against a self-hosted local
dashboard, pick the first concrete defect with operator impact, file and
claim a focused bead, fix it, and reintegrate. The chosen slice is a
narrow-viewport UX defect where filter-chip rows hide their horizontal
scroll affordance entirely on phone-sized layouts.

## Bead(s)

- `bd-34a693` — [caco-web] filter-chip rows hide overflow scroll affordance
  on narrow viewports (claimed & implemented this cycle)

## Before state

- Failing tests: 5 pre-existing caco-web lib failures on `origin/main`
  (`tests::app_js_nodes_view_has_master_detail_and_node_scoped_chat_bd_5f3d5c`,
  `tests::app_js_nodes_view_treats_macos_load_as_informational_bd_eb9999`,
  `tests::app_js_uses_api_unwrap_helper_for_success_envelope_sites`,
  `tests::summaries_background_loads_remaining_pages_bd_2dd008`,
  `tests::summaries_loading_validates_and_retries_failures_bd_37188a`).
  Confirmed broken-on-main via clean-stash bisect; broadcast and not
  adopted in this cycle.
- Playwright evidence at 390x844: `#agent-state-chips` rendered w=358 with
  scrollWidth=946; `#bead-status-chips` w=358 / sw=833. The narrow-viewport
  CSS rule explicitly hides the scrollbar (`scrollbar-width: none`,
  `::-webkit-scrollbar { display: none; }`), so chips like Permanent /
  Draft / Failed / Completed were invisible and undiscoverable.
- Connection state at observation time: `Snapshot delayed` due to
  self-launched daemon-less local instance; routes still rendered enough
  DOM to capture overflow geometry.

## After state

- Failing tests: identical 5 broken-on-main failures; my diff adds none.
  395 passing tests unchanged.
- `cargo check -p caco-web --all-targets` finishes clean (1m 51s warm).
- `.filter-chip-row` at `@media (max-width: 768px)` now masks the
  trailing edge with a `linear-gradient` so the last chip fades into the
  background, and toggles `at-start` / `at-end` / `at-both` /
  `no-overflow` classes via `attachFilterChipOverflowAffordance()` based
  on live `scrollLeft` / `scrollWidth` / `clientWidth`. Listeners are
  wired once per element (`__chipAffordanceWired` flag).

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Files touched:
  - `crates/caco-web/static/style.css` (~L4335): narrow-viewport
    `.filter-chip-row` block extended with mask-image affordance and
    `.at-start` / `.at-end` / `.at-both` / `.no-overflow` variants.
  - `crates/caco-web/static/app.js`: added
    `attachFilterChipOverflowAffordance(wrap)` helper; called from
    `renderAgentStateChips()` and `renderBeadStatusChips()` after
    `innerHTML` replacement.
- Tests: +0 / -0 / flipped 0. Defect is visual-CSS plus DOM behaviour;
  Playwright evidence is captured under the summary directory rather
  than a new lib test (avoids touching the broken-on-main test surface).
- Behavioural delta: on narrow viewports the chip row is visibly
  scrollable (right-edge fade) and the fade tracks scroll position so
  the operator can tell from the row chrome that more filters exist
  off-screen.

## Embedded artefacts

- `web/observation.log` — full caco-web-observe comprehensive pass log
  (routes, snapshots, DOM probes, overflow geometry).
- `web/server.log` — self-launched caco-web server log during the pass.
- `web/console.log` — copied chromium console log for the pass.
- `web/screenshots/page-2026-05-20T01-25-33-649Z.png` etc — representative
  screenshots showing chip-row overflow before fix.

## Operator-takeaway

The dashboard's `scrollbar-width: none` polish on narrow viewports
silently amputated discoverability of half the filter chips. The shape
of the bug is general — any horizontally-scrollable row with a hidden
scrollbar needs an explicit edge affordance — so the
`attachFilterChipOverflowAffordance` helper is the natural primitive
to reuse for future chip/pane rows (e.g. command palette tabs, view
switchers) rather than duplicating gradient masks across components.
