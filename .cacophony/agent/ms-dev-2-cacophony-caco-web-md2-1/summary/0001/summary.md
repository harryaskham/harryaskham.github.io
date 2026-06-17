# Session summary — caco-web Beads list horizontal-overflow scroll affordance

## Goal

Fix the operator-facing caco-web Beads-list defect where, at desktop width, the
rightmost columns (Created / Modified) are silently clipped at the content edge
with no obvious way to reach them — the table needs ~1360px of columns inside a
~1116px desktop content area. Make the overflow read as a clear, accessible
horizontal scroll affordance instead of a broken-looking hard cut-off.

## Bead(s)

- `bd-bf9ed1` — caco-web: Beads list LABELS chips clip mid-word and CREATED column overflows at desktop width

## Before state

- Failing tests: none (static-asset visual bug).
- Label chips: the bead reported chips hard-clipping mid-word; live inspection
  showed this half was ALREADY fixed (bd-e19e15 inner `.label-tag-txt`
  truncation span → ellipsis + full-label title tooltip). The bead's evidence
  screenshot was from an older build (caco 1.2.1261).
- Table overflow: confirmed live at 1440px — the `.table-wrapper` is correctly
  contained (right edge 1394, within the view's 36px padding) and has
  `overflow:auto`, but the table is 1364px wide in a 1116px area, so the Created
  header rendered as "CREATE", Created values jammed the visible edge, and
  Modified was entirely off-screen with no obvious scroll affordance.

## After state

- Failing tests: none. `node --check app.js` clean; `cargo test -p caco-web --lib`
  passed via the daemon test queue (exit 0), including the `style_css_*`
  CSS-structure tests.
- Added an adaptive horizontal scroll-shadow to the Beads `.table-wrapper`
  (`.scroll-hint-x`): the visible edge fades toward off-screen content, toggling
  `at-start` (fade right) → `at-end` (fade left) → `no-overflow` (no fade) from
  the live scroll position via the existing `attachFilterChipOverflowAffordance`
  helper. Verified: initial state fades the right edge (Created/Modified read as
  "scroll right for more"); after scrolling, both Created and Modified are fully
  reachable and the left edge fades. Console clean (0 messages) at 1440px.
- Label-chip half already correct (ellipsis + tooltip); no change needed there.

## Diff summary

- Code/content commit: pending final squash SHA from the reintegration receipt.
- Files touched:
  - `crates/caco-web/static/style.css` — new additive `.scroll-hint-x`
    at-start/at-end/at-both/no-overflow mask rules (mirrors the bd-34a693
    `.filter-chip-row` affordance), inserted after the canonical `.table-wrapper`
    block (existing block unmodified).
  - `crates/caco-web/static/index.html` — added `scroll-hint-x` class to the
    Beads table-wrapper only.
  - `crates/caco-web/static/app.js` — wired `attachFilterChipOverflowAffordance`
    on the Beads wrapper after `enableColumnResize('beads-table')`.
- Tests: +0 / -0; validated via measured DOM probes (overflow + scroll-class
  toggling) and before/after screenshots. Existing `style_css_*` structure tests
  remain green (additive-only CSS, single `.table-wrapper {` rule-head preserved).
- Behavioural delta: the wide Beads table's overflowed rightmost columns now have
  an adaptive edge-fade indicating horizontal scrollability instead of a silent
  clip; no columns hidden, no data lost.

## Embedded artefacts

- `web/screenshots/before-beads-1440.png` — pre-fix: Created cut to "CREATE",
  values jammed at the viewport edge, Modified off-screen, no scroll cue.
- `web/screenshots/after-beads-1440.png` — post-fix initial: right edge fades
  (at-start) signalling more columns to the right.
- `web/screenshots/after-beads-1440-scrolled.png` — post-fix scrolled to end:
  Created + Modified fully visible, left edge fades (at-end).
- `web/screenshots/beads-probe.json`, `web/screenshots/after-probe.json` —
  DOM measurements (wrapper/table widths, overflow, label-chip clip+title state,
  scroll-class toggling, console tails).

## Operator-takeaway

The Beads list's rightmost columns are no longer silently cut off at desktop
width: a calm, adaptive edge-fade now signals the table scrolls horizontally, and
Created/Modified are reachable. The label-chip-clip half of the bead was already
fixed by bd-e19e15, so only the table-overflow half needed work. Reusing the
existing `.filter-chip-row` overflow-affordance helper kept the change small and
consistent. Landed during the window where the merge gate is disabled (Harry's
merge-train posture), so local validation (node --check + queued caco-web lib
tests) stood in for the gate.
