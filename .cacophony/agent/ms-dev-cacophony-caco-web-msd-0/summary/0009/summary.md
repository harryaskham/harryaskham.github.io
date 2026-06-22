# Session summary — caco-web mobile hero canvas dim (bd-c5d2b1)

## Goal

Continue the caco-web duty cycle with a different-tactic probe (narrow-responsive
stress) after the prior broad console+visual pass came back clean. Found and
fixed a mobile-specific legibility defect: on narrow viewports the Status
homepage cluster-pulse hero collapses to one column, so the foreground copy/pills
sit over the full-opacity animated canvas and its node labels collide with the
foreground text.

## Bead(s)

- `bd-c5d2b1` — caco-web Status hero: cluster-pulse canvas collides with
  foreground pills/subtitle on narrow/mobile viewports (no narrow-viewport canvas
  dim). Filed + claimed during this narrow-responsive observation probe (P3 bug;
  labels: caco-web, web, visual-polish, responsive, legibility).

## Before state

- Failing tests: none (CSS-only; no Rust/JS/tests touched).
- Defect: at 390px the hero collapses to a single column; the foreground copy
  block + meta pills ("● Live", "⌘K command palette", "/ focus search") sit
  directly over the full-opacity `.cluster-pulse-canvas`. The canvas node labels
  (po2/hel/ast/msd/sgu… with bd-a9cf66 backing plates) bleed past the bd-cdbea4
  copy/meta scrim's radial fade and collide with the foreground text
  (text-over-text). Vision pass independently flagged the collision.
- Existing `@media (prefers-reduced-motion: reduce)` dims the canvas to 0.55, but
  there was NO narrow-viewport equivalent — normal-motion mobile renders the
  canvas at full opacity behind the stacked content.
- Console: clean.

## After state

- Failing tests: none.
- Fix: added `@media (max-width: 640px) { .status-hero .cluster-pulse-canvas {
  opacity: 0.4; } }` (style.css, +10 lines incl. comment) — dims the hero canvas
  on mobile so the single-column foreground content dominates, matching the
  reduced-motion dim precedent. Scoped to `.status-hero` so the Nodes-view pulse
  canvas + the expanded hero overlay are unaffected.
- Validation: Playwright at 390px confirmed computed `opacity: 0.4` on the hero
  canvas; vision pass confirmed "foreground pills and subtitle clearly legible on
  opaque dark pill backgrounds; particle/node-label backdrop dimmed and receded,
  visible only in negative space, does not overpower the foreground text" — the
  collision is resolved. Console clean.

## Diff summary

- Code commit: final landed squash SHA comes from the reintegration receipt
  (summary artefact SHA intentionally not self-referenced).
- Files touched: `crates/caco-web/static/style.css` (+10 / -0) — one scoped
  narrow-viewport media query. No Rust, no JS, no tests.
- Behavioural delta: at ≤640px the hero canvas recedes so foreground content is
  legible; desktop/wide layout unchanged (copy left, canvas right — no overlap);
  Nodes-view pulse + expanded overlay unaffected.

## Embedded artefacts

- `web/screenshots/before-narrow-collision.png` — 390px hero, full-opacity canvas
  colliding with the foreground pills/subtitle.
- `web/screenshots/after-narrow-dimmed.png` — 390px hero, canvas dimmed to 0.4,
  foreground content clearly legible.

## Operator-takeaway

The hero's legibility treatments (bd-cdbea4 copy/meta scrim, bd-a9cf66 node-label
plates) were all tuned on the desktop two-column layout where copy and canvas
sit side-by-side. The mobile single-column collapse stacks them, so the canvas
backdrop needed its own narrow-viewport dim — a class of responsive gap that only
surfaces under a deliberate narrow-width probe, not a desktop pass. Landed via
the controller-authorized JS/CSS-only --skip-hooks fast path during the bd-0ffc21
reintegration congestion.
