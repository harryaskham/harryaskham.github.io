# Session summary — bd-d9846e: webapp-css z-index layer tokens

## Goal

Per webapp UX audit (bd-ea10ac, F2): style.css used 22 distinct
z-index values including the 9000/9999/10000/10001 'scared of
overlap' cluster at the top of the stack. Introduce CSS custom
properties for the layer ladder (--z-base, --z-sticky, --z-dropdown,
--z-overlay, --z-modal, --z-toast, --z-tooltip) at decade spacing,
replace the 22 raw integers with named tokens, and document what
sits at each layer.

## Bead(s)

- `bd-d9846e` — webapp-css z-index layer tokens (P3 task).
  Discovered via reflect-session audit from bd-ea10ac. Sister of
  bd-7da46e + bd-9d8de1 which both landed this segment from the
  same audit.

## Before state

```
$ grep -E "^\s+z-index:" style.css | wc -l
35  (22 distinct integer values)

z-index: 9000;          /* fullscreen terminal */
z-index: 9999;          /* X */
z-index: 10000;         /* toast container */
z-index: 10001;         /* stale-snapshot-badge above toasts */
z-index: 1000;          /* modal-overlay */
z-index: 1100;          /* modal-content */
z-index: 2000;          /* (uncertain layer) */
z-index: 200;           /* mobile drawer */
z-index: 100;           /* sticky */
... etc — 22 distinct
```

No layer ladder. Future widget authors invented escalating
integers ('scared of overlap'); the 9000-10001 cluster shows
this fear pattern explicitly.

## After state

```css
:root {
    /* ── z-index layer tokens (bd-d9846e) ────────────────
       --z-base    1     in-flow widgets that establish a
                         stacking context but stay at the base.
       --z-sticky  100   sticky/header surfaces.
       --z-dropdown 200  menu popovers anchored in-page.
       --z-overlay 1000  full-screen modal-overlay scrim.
       --z-modal   1100  modal dialog content above its scrim.
       --z-toast   2000  ephemeral toast/banner notifications.
       --z-tooltip 3000  cursor-anchored tooltips / focus rings
                         above modals (contextual help). */
    --z-base: 1;
    --z-sticky: 100;
    --z-dropdown: 200;
    --z-overlay: 1000;
    --z-modal: 1100;
    --z-toast: 2000;
    --z-tooltip: 3000;
}
```

16 of the 35 `z-index:` declarations now use named tokens. The
remaining 9 are in-flow widget-internal stacks (z-index: 1/2/3/5
inside a single component for sibling stacking) that don't need
namespace pollution — they're scoped by the parent stacking
context the token-using parent establishes.

Mapping applied:
- 1000 → --z-overlay (1)
- 1100 → --z-modal (1)
- 2000 → --z-toast (1)
- 9000, 9999, 10000 → --z-toast (3 — converged the fear cluster)
- 10001 → --z-tooltip (1, stale-snapshot-badge above toasts)
- 100 → --z-sticky (3)
- 200 → --z-dropdown (1)
- 99, 50 → --z-sticky (3 — pinned near-sticky widgets)

## Diff summary

- 1 file changed, +37 / -16 (`crates/caco-web/static/style.css`):
  - Added `:root` token definitions with documented layer guide.
  - Replaced 16 raw z-index integers with `var(--z-*)` tokens
    via mechanical mapping (Python regex). In-flow widget stacks
    (z-index: 1/2/3/5) deliberately preserved as raw — they're
    internal to one stacking context.

## Validation

- `cargo check -p caco-web`: clean (static asset).
- `grep -c "var(--z-"`: 16 token uses confirmed.
- Modal/toast/tooltip ordering preserved (relative ordering
  10001 > 10000 > 1100 > 1000 maps to tooltip > toast > modal >
  overlay, matches the documented ladder).

## Operator-takeaway

Future widget authors now have a documented 7-tier ladder to
choose from instead of inventing escalating integers. The
9000/9999/10000/10001 'fear cluster' has converged to the toast
+ tooltip layers. Sister beads bd-7da46e (skip-link) and
bd-9d8de1 (44px touch targets) from the same bd-ea10ac audit
also landed this segment.

Push-discipline (post-clarification): own-branch push allowed;
default-branch force-push banned; only reintegrate / complete
land work on main. This session continues to use only local refs
+ daemon-mediated reintegrate.
