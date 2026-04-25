# Session summary — caco-web cluster pulse graph regression fix

## Goal

Fix the live caco-web cluster-pulse regression reported by Harry: the expand button still looked broken, and the animated node graph could appear twice and freeze when navigating or expanding the graph.

## Bead(s)

- `bd-00ab45` — caco-web cluster pulse expand button and node graph animation still broken
- related permanent owner context: `bd-5bfb2c` — caco-web workspace/polish ownership

## Before state

- Failing tests: none known; the regression was visual/operator-reported in the live webapp.
- Relevant metrics: existing source tests pinned the too-subtle 18px / low-opacity expand control and did not cover the real `status` route or View Transitions interaction with the live canvas.
- Context: `bd-896551` Session Summaries was separately assigned to msm-5; this session intentionally avoided that surface.

## After state

- Failing tests: none observed in targeted validation.
- Relevant metrics: cluster-pulse source tests now cover the accessible expand button, restored 32px visible click target, status/nodes canvas routing, modal teardown through WorkspaceOverlay, and bypassing View Transitions whenever the live graph canvas is involved.
- Context: the live graph now remounts for the actual default `status` view as well as `nodes`, and modal/navigation teardown closes through the overlay handle instead of only removing CSS classes.

## Diff summary

- Commits: `1b99760d2`
- Files touched: `crates/caco-web/static/app.js`, `crates/caco-web/static/index.html`, `crates/caco-web/static/style.css`, `crates/caco-web/src/tests.rs`
- Tests: `cargo fmt --all -- --check`; `git diff --check`; `cargo test -p caco-web cluster_pulse --lib`; `cargo check -p caco-web --tests`
- Behavioural delta: cluster-pulse transitions no longer use browser View Transitions when the live canvas is involved, the default status canvas is a first-class remount target, fullscreen close/nav teardown runs through the shared overlay helper, and the expand button is visibly clickable again.

## Operator-takeaway

The fix targets the live UX complaint directly: the cluster graph should no longer duplicate/freeze across status/nodes/modal transitions, and the expand control should read as an actual button instead of a barely visible decorative glyph.
