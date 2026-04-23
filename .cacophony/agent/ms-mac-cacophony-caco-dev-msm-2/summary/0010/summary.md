# Session summary — Cluster-pulse visual hierarchy (bd-d32405)

## Goal

Restore visual hierarchy in the homepage hero: the animated
cluster-pulse graph should be the prominent element; the
expand button should recede until hovered.

## Bead(s)

- `bd-d32405` — Make cluster pulse expand button more subtle
  and increase graph animation prominence (P2, cluster-pulse,
  ui, visual-design)

## Before state

- `.cluster-pulse-canvas` rendered at `opacity: 0.95` with a
  `linear-gradient(180deg, transparent 60%, rgba(15,19,24,0.55))`
  bottom-fade overlay, muting the lower 40% of the graph.
- `.cluster-pulse-expand-btn` was 28x28 with full color
  (`rgba(229,233,240,0.72)`) and a `translateY(-1px)` /
  `box-shadow` lift on hover — visually competitive with the
  graph it controls.

## After state

- Canvas at `opacity: 1` and overlay reduced to a single light
  radial accent (no bottom-darken fade) — the animation now
  reads cleanly across the full hero area.
- Button shrunk to 22x22 with base `opacity: 0.55` + softer
  base color (`rgba(229,233,240,0.42)`); recedes until either:
  - the hero section is hovered (`opacity: 0.85` via
    `.status-hero:hover .cluster-pulse-expand-btn`)
  - the button itself is hovered (`opacity: 1` + accent color
    + softer background tint, no transform-lift)
  - the button has focus (`opacity: 1` + accent outline,
    a11y preserved)
- SVG icon shrunk 14px → 12px to match the smaller hit target.

Affordance preserved across all three interaction paths (hero
hover, button hover, focus); a11y outline unchanged.

## Diff summary

- Files touched:
  - `crates/caco-web/static/style.css` — bd-d32405 edits to
    `.cluster-pulse-canvas`, `.cluster-pulse-overlay`, and
    `.cluster-pulse-expand-btn` (+ `:hover` / `:focus-visible`
    siblings)
  - `crates/caco-web/src/tests.rs` —
    - Updated existing `style_css_has_cluster_pulse_styles`
      to assert the new 22px sizing
    - Added regression test
      `cluster_pulse_visual_hierarchy_emphasises_graph_over_control`
      that block-parses both rules and asserts: canvas
      opacity 1, button 22x22 + base opacity 0.55, hero-hover
      ramp selector present
- Tests: +1 / -0 / flipped 1 (sizing assertion in existing
  test updated)
- Test command: `cargo test -p caco-web cluster_pulse`
  → 5 passed, 0 failed.

## Operator-takeaway

Three knobs you can flip if the new visual reads wrong:

1. **Hero section hover ramp opacity** — currently 0.85 in
   `.status-hero:hover .cluster-pulse-expand-btn`. Lower for
   more recessive; raise toward 1.0 for snappier reveal.
2. **Button base opacity** — currently 0.55. Lower (0.4) to
   make it nearly invisible until hovered; raise (0.7) to
   keep it more present.
3. **Overlay darken-fade** — removed in this bead because it
   muted the canvas. If hero text legibility regresses on
   light-content snapshots, re-add a milder
   `linear-gradient(180deg, transparent 70%, rgba(15,19,24,0.25))`
   to the overlay rule.

The regression test pins the contract (canvas reads at
opacity 1, button base opacity 0.55, 22x22 with hero-hover
ramp). Future operators can change the values freely as long
as the contract holds; flipping the hierarchy will fail the
test loudly.

Honored constraints:
- No `cargo test --workspace`; targeted to `cluster_pulse`
  filter (5 tests).
- No daemon / sidecar touch (caco-tui agent's compile break
  on `TopLevelBeadsConfig::peer_consult_timeout_ms` remains
  their fix, not mine).
- Operator no-narrator rule honored — claim + close speaks
  issued by msm-2 directly.

13th bead closed this session (cumulative across both turns).
