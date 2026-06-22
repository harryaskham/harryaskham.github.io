# Session summary — caco-web: fix sidebar Agents submenu layout (bd-604c14)

## Goal

Operator (Harry) filed bd-604c14 (P2, screenshot): the caco-web sidebar Agents
section status-filter tabs (Active/Failed/Completed/All/Artefacts) render clipped
('Activ…','Comp…','Artef…'), pushed to the right gutter, stacked vertically with
big gaps, badges/icon misaligned vs the clean Beads/Feed rows. Routed to me.

## Bead(s)

- `bd-604c14` — caco-web sidebar Agents submenu layout bug (claimed + fixed)

## Before state

- Failing tests: none.
- Root cause (confirmed live-DOM): the `.nav-submenu` <ul> is a CHILD of the
  `display:flex .nav-item` row (alongside icon/label/badge/key/toggle). `.nav-item`
  was `flex-wrap: nowrap` and `.nav-submenu` had no full-width basis, so when expanded
  the submenu became a flex item squeezed into the ~58px right-gutter remainder
  (probe: submenu offsetWidth=58, offsetLeft=128) → its items' 45px left padding
  pushed the labels off the right edge, clipped + stacked vertically.

## After state

- Failing tests: none (`cargo test -p caco-web --lib`, tj-10287199 + new contract
  test `nav_submenu_claims_full_width_row_bd_604c14`).
- Fix: `.nav-item { flex-wrap: wrap; gap: 0 11px; }` (allow wrap, no row-gap so the
  collapsed submenu adds no vertical space) + `.nav-submenu { flex-basis: 100%; }`
  (claim its own full-width row below the nav line).
- Live-DOM validated: submenu now offsetWidth=173, top=30 (its own row), main
  "Agents" row stays on line 1 (label top=9); all 5 visible labels fit
  (labelClip=false for Active/Failed/Completed/All/Artefacts); screenshot confirms
  a clean compact vertical list with aligned badges, consistent with Beads/Feed.
- Note: the Artefacts item-level scrollWidth reads 227 (vs 173) ONLY because of its
  long `data-tooltip` rendered via the global `[data-tooltip]::after` (hidden until
  hover, absolutely positioned) — NOT visible content; the visible label is not
  clipped (labelClip=false) and the screenshot confirms it.

## Diff summary

- Code/content commit: pending final squash SHA from the reintegration receipt.
- Files touched:
  - `crates/caco-web/static/style.css`: `.nav-item` flex-wrap:wrap + gap:0 11px; `.nav-submenu` flex-basis:100%.
  - `crates/caco-web/src/tests.rs`: +1 contract test (bd-604c14).
- Tests: +1 / -0 / flipped 0.
- Behavioural delta: sidebar Agents submenu wraps to its own full-width row — labels
  fit (no clipping), vertically compact, badges aligned; other nav items unchanged.

## Embedded artefacts

- `web/screenshots/sidebar-broken.png` — reproduced broken state (labels clipped, squeezed right).
- `web/screenshots/sidebar-fixed.png` — fixed state (all labels visible, compact, aligned).

## Operator-takeaway

A clean operator-filed P2 layout regression: the agents submenu was a flex child of
the horizontal nav row with no full-width basis, so it squeezed into the right
gutter. Two CSS lines (flex-wrap + flex-basis:100%) restore the proper full-width
wrapped row. Reproduced + validated end-to-end with the live-DOM tooling.
