# Session summary — caco-web inactive workspace route fix

## Goal

Fix a Playwright-observed caco-web layout regression where the Workspace route remained visible below the active Status route, causing workspace pane controls to leak into the operator-facing Status page.

## Bead(s)

- `bd-457975` — caco-web inactive workspace view remains visible below active route

## Before state

- Failing tests: none known at session start.
- Relevant metrics: active duty cycle found no assigned or ready caco-web bead, then Playwright observation against current assets showed `#view-workspace` had `display: flex` and a non-zero `1200x500` rect while Status was the active route.
- Context: the generic `.view { display: none; }` rule was overridden by a base `#view-workspace { display: flex; }` rule, so Workspace was visible even when it was not the active view.

## After state

- Failing tests: none in caco-web validation.
- Relevant metrics: Playwright confirmed inactive `#view-workspace` now has `display: none` and zero rect on `/#status`, and pressing `w` activates Workspace with `display: flex`; `cargo check -p caco-web --all-targets` passed; `cargo test -p caco-web --lib` passed with 281 tests.
- Context: only the active workspace selector now sets `display: flex`; the base workspace selector keeps layout direction/height without overriding inactive route hiding.

## Diff summary

- Commits: `0f76fb23d`
- Files touched: `crates/caco-web/static/style.css`, `crates/caco-web/src/tests.rs`
- Tests: +1 regression test / -0 / flipped 0
- Behavioural delta: inactive workspace no longer leaks into Status/other routes, while the `w` shortcut still reveals the Workspace route normally.

## Embedded artefacts

- `.playwright-cli/page-2026-04-26T16-53-14-360Z.png` — before evidence showing the workspace view visible below Status.
- `.playwright-cli/page-2026-04-26T16-58-33-419Z.png` — after smoke showing Workspace activates only when selected.

## Operator-takeaway

A single route-specific CSS override made Workspace behave unlike every other dashboard view. The fix restores the shared route visibility contract and adds a regression test so Workspace cannot silently reappear under other pages.
