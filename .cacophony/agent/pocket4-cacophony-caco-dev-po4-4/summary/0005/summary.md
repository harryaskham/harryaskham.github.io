# bd-2f19b9: redesign cluster-pulse-expand-btn styling

## Goal

Operator called out the cluster-pulse-expand-btn as opaque grey and
overly wide; redesign it to be smaller, more translucent, and animate
smoothly so it doesn't visually compete with the animated graph it
lives on top of.

## Bead(s)

- bd-2f19b9 (this commit)

## Before state

Button sat at 22x22 with `opacity: 0.55`, `background: transparent`,
and a single transition keyword `var(--transition)` applied to every
property. Backdrop-filter was 6px. No transform-scale feedback, no
prefers-reduced-motion path, no press-in. The button read as a flat
grey block on top of the cluster-pulse graph.

## After state

- 18x18 (smaller), SVG child 10x10
- Base opacity 0.38 with `rgba(46,52,64,0.12)` fill so the backdrop
  blur has something to work with without looking flat
- Backdrop-filter 8px + -webkit prefix for Safari parity
- Transform-scale choreography:
    0.94 (base) → 1.0 (hero hover) → 1.08 (direct hover) → 0.96 (press)
- Explicit 220ms cubic-bezier(0.2, 0, 0.2, 1) applied to
  color/background/border/opacity/transform/box-shadow; active-state
  overrides duration to 80ms so the click reads as snappy
- `will-change: transform, opacity` hints the compositor
- `@media (prefers-reduced-motion: reduce)` disables both transitions
  and the scale so the accessibility affordance is preserved

## Diff summary

- `crates/caco-web/static/style.css`: ~42 lines changed (whole rule
  plus reduced-motion block)
- `crates/caco-web/src/tests.rs`: 3 assertion edits —
    width:22px → width:18px
    opacity:0.55 → opacity:0.38
    +new assertion that the rule contains `cubic-bezier` + `transform`
  so a future regression can't quietly drop the smoothing
- caco-web lib tests: 5/5 cluster_pulse-related tests pass
- `cargo check --workspace --tests`: clean

## Operator-takeaway

Button is now visually subordinate to the graph (smaller + translucent
+ blurred background) while still being reachable via hero-hover ramp
and direct hover. The animation reads as one motion thanks to a single
shared easing curve, and users with reduced-motion preferences get a
static button instead of a broken-looking one. No JS changes — purely
CSS + test pins, so there's no runtime cost or new API surface.
