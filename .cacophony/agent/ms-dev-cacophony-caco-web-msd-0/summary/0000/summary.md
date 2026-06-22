# Session summary — caco-web Status hero legibility fix (bd-cdbea4)

## Goal

Run a caco-web observation duty cycle on ms-dev: stand up the dashboard, drive it
with Playwright, sweep the operator-visible views for jank/regressions, and land a
focused improvement. The cycle found and fixed a real legibility defect in the
Status view hero, where the "Cluster pulse at a glance" copy and meta pills were
rendered illegibly on top of the full-opacity animated cluster-pulse canvas.

## Bead(s)

- `bd-cdbea4` — caco-web: Status hero copy/meta text is unreadable over the
  full-opacity cluster-pulse canvas (missing documented scrim) — filed, claimed,
  implemented, validated, landed this cycle.
- `bd-9dd01f` (existing draft) — appended a concrete NixOS @playwright/cli
  executablePath config recipe as an addendum (reflect-session friction; did not
  file a duplicate).

## Before state

- Failing tests: none known.
- Installed caco 1.2.1261; checkout rebased onto origin/main (was 6 behind).
- Dashboard launched ad-hoc on 127.0.0.1:19411 against daemon 127.0.0.1:11100.
- Broad Playwright pass across 19 main views: console 0 messages, 0 failed
  network requests; all 21 advertised keyboard shortcuts navigate correctly; no
  document-level horizontal overflow at 390px.
- Defect: `.cluster-pulse-canvas` is absolute/inset:0/opacity:1/mix-blend:screen.
  `.status-hero-copy` / `.status-hero-meta` (z-index:2) had NO background despite
  bd-d32405's comment claiming the copy block "sits at z-index:2 with its own
  background contrast." Hero heading/eyebrow/summary and meta pills overlapped
  the canvas node labels — low contrast at 1440px, unreadable at 390px (where the
  hero collapses to one column over the densest part of the graph).

## After state

- Failing tests: none known. CSS is brace-balanced; `git diff --check` clean.
- `.status-hero-copy::before` / `.status-hero-meta::before` add a soft localized
  radial scrim (rgba(15,19,24) fading to transparent), and copy children + meta
  pills get a `text-shadow` halo. Text is legible at both 1440px and 390px while
  the animated graph remains visible as the primary visual (honors bd-d32405).
- Validation: injected the exact CSS into the live dashboard via Playwright and
  captured before/after at wide + narrow — screenshots/fix/{before-wide,
  after-wide,after-narrow}.png confirm the rendered result. Heavy compile is left
  to the merge-queue gate (CSS-only static-asset edit cannot affect Rust
  compilation; host was under load with a visible merge-queue retry storm).

## Diff summary

- Code/content commits: pending final squash SHA from the reintegration receipt.
- Files touched: `crates/caco-web/static/style.css` (+25 lines, one block added
  after the `.status-hero .status-hero-copy/.status-hero-meta` z-index rule).
- Tests: +0 / -0 (visual CSS change; validated by Playwright before/after).
- Behavioural delta: Status hero copy/meta text is now legible over the animated
  cluster-pulse canvas at all widths; animation visibility preserved.
- Gate-fix: the meta text-shadow selector is `.status-hero-meta > *` (not
  `.status-hero-meta .hero-pill`), so it does not introduce an earlier
  `.hero-pill {` token that would shadow the naive `find(".hero-pill {")` in the
  existing `style_css_wraps_status_hero_pills_on_narrow_degraded_views_bd_761cc4`
  test. Verified the bd-761cc4 assertions pass against the edited CSS.

## Embedded artefacts

- `web/audit.md` — route checklist, broad-pass result, shortcut/responsive notes.
- `web/screenshots/00-status.png`, `web/screenshots/narrow/narrow-status.png` —
  before-state defect (desktop bleed + narrow unreadability).
- `web/screenshots/fix/before-wide.png`, `after-wide.png`, `after-narrow.png` —
  fix proof.
- `web/screenshots/view-02-beads.png`, `view-08-files.png`,
  `view-19-merge-queue.png`, `narrow/narrow-beads.png` — representative views.
- `web/shortcut-test.txt` — all 21 shortcuts pass.
- `web/console-*.txt`, `web/requests-*.txt`, `web/server.log` — clean console/network evidence.

## Operator-takeaway

The Status hero's "graph as primary visual" design (bd-d32405) removed the
full-canvas darken overlay on the promise that the copy block would carry "its
own background contrast" — but that backing was never actually applied, so the
most-seen surface in the dashboard had unreadable hero text, badly on phones.
This cycle restores that documented localized scrim. If the cluster-pulse hero is
restyled again, keep a localized text-contrast backing (scrim or shadow) on the
z-index:2 copy/meta blocks rather than relying on a full-canvas overlay.
