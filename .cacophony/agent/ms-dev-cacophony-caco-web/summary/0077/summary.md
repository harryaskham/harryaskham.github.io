# Session summary — bd-29e9b1: content-visibility:auto on .summaries-row

## Goal

Extend the off-viewport-skip primitive to another
high-value unbounded-list target. Looking for next-best
candidate after `.inbox-card` (bd-57c0f5).

## Bead(s)

- `bd-29e9b1` — [caco-web] add content-visibility:auto + contain-intrinsic-size:78px to .summaries-row

## Candidate selection

Surveyed remaining repeating-list selectors:

- `.chat-message` — variable size 50-500px, bare-`<length>`
  contain-intrinsic-size would cause scroll-jump when
  rows expand. **Risky** without `auto <length>` form
  (Chrome 102+ / Safari 17.4+) which this codebase
  intentionally avoids for Safari 18.0 baseline.
- `.node-card` — typical clusters have <10 nodes; not
  unbounded enough to matter.
- **`.summaries-row`** — unbounded growth (every
  reintegration across every project across every agent),
  consistent size floor (`min-height: 4.25rem`), no focus
  traps, viewport-bound interactions only. **Selected.**

## Why .summaries-row is safe

Each row's box math:

| Component | Height |
|---|---|
| padding (0.8rem × 2) | ~25.6px |
| head line | ~16px |
| title (2-line clamp) | ~28-40px |
| meta row | ~14px |
| **typical total** | **~85-95px** |
| min-height floor | 68px |

Reserved 78px — slightly above min-height, well below
typical max. Off-viewport rows re-flow on activation.

The `.summaries-row-revealed` 900ms reveal animation runs
only after `scrollIntoView` places the freshly-added row
in viewport, so containment doesn't impact it.

## Fix

Added `content-visibility: auto; contain-intrinsic-size: 78px;`
at the top of the `#view-summaries .summaries-row` block
in `summaries.css`. Bare-`<length>` form per the
established Safari 18.0 baseline convention.

## Test design (cross-stylesheet aware)

Two layers:

1. **Brace-depth block scoping** (bd-57c0f5 established
   pattern) — assert BOTH primitives live inside the
   `#view-summaries .summaries-row` body specifically.
2. **Cross-stylesheet total-count** — iterates 8
   stylesheets (style.css + summaries.css + 6
   workspace-\*.css files) and sums
   `content-visibility: auto` occurrences. Requires
   >= 6 (5 pre-existing + this one). Future
   delete-everywhere sweep across any file fails the
   assertion.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted to avoid self-referential SHA.
- Files touched:
  - `crates/caco-web/static/summaries.css` -- 15-line addition (13 comment + 2 declarations) at top of #view-summaries .summaries-row block.
  - `crates/caco-web/src/tests.rs` -- regression test with cross-stylesheet aware total-count.
  - `.cacophony/agent/ms-dev-cacophony-caco-web/summary/pending/web/*` -- bounded validation receipts.
- Tests: +1 caco-web static asset regression test. Net pass count: 483 -> 484; 11 pre-existing failures on main unchanged.

## Operator-takeaway

Operators with long-lived dashboards (weeks of accumulated
session summaries across the cluster) now pay summaries-
list layout/paint cost proportional to **viewport size**
instead of **total summary count**. Sixth selector to use
the primitive across the codebase; the pattern is now
firmly established for repeating-unbounded lists.
