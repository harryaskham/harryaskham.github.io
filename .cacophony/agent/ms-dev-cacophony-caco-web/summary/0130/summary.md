# Session summary — bd-cf15b7: fix broken aria-controls + add aria-idref integrity test (pattern x #3)

## Goal
Apply pattern (x) continuous-defense-test to a third audit category: ARIA idref integrity. Find any aria-* attributes pointing to non-existent ids.

## Bead
- `bd-cf15b7`

## Bug found and fixed
`aria-controls="cluster-pulse-expanded-overlay"` on the cluster-pulse expand button (index.html L301) pointed at an element that was lazy-created in `app.js openClusterPulseOverlay()`. Screen readers couldn't navigate via aria-controls until first open.

**Fix**:
1. Pre-rendered empty placeholder `<div id="cluster-pulse-expanded-overlay" class="cluster-pulse-expanded-overlay" aria-hidden="true"></div>` before `</body>`.
2. Modified `openClusterPulseOverlay()` in app.js: dual-path branch detects empty pre-rendered shell and populates `innerHTML` lazily on first open (preserves cold-load deferral while making the idref resolve from page load).

## Audit results (7 refs total)
- 6 OK (statically resolved): `agents-submenu`, `cluster-pulse-canvas`, `command-palette-results`, `sidebar`, `agent-detail-title`, `bead-detail-title`.
- 1 BROKEN: `cluster-pulse-expanded-overlay` — fixed by this bead.

## Pattern (x) third instance — `index_html_aria_idref_integrity_bd_cf15b7`
- Scans index.html for `aria-(controls|labelledby|describedby|owns)="<id>"` refs.
- Handles space-separated id-list values (aria-labelledby supports multiple ids).
- For each id token, looks up `id="<token>"` in static index.html.
- `ALLOWLIST_DYNAMIC_ARIA_TARGETS` placeholder for genuinely-dynamic targets.
- Forward-guards: cluster-pulse-expanded-overlay placeholder must remain + must remain empty + app.js dual-path branch must remain intact.

## Pattern (x) generalization (3 audit categories now covered)
1. CSS custom properties (bd-91616b — pruned 2 + defended).
2. @keyframes (bd-a753e2 — 0 prune + defended).
3. ARIA idrefs (bd-cf15b7 — **fixed 1 broken** + defended).

**Subtraction direction inversion noted**: bd-91616b/bd-a753e2 subtract `defs - consumers`; bd-cf15b7 subtracts `consumers - defs`. Both forms are valid pattern (x) shapes — depends on whether the declarative or consumer space is the integrity-critical one.

## Operator-visible effect
- Screen-reader users navigating the cluster pulse expand button via aria-controls now successfully reach the controlled overlay from page load instead of waiting for first click.
- Future devs adding aria-* refs to non-existent ids see CI fail with "found N broken aria idrefs in index.html..." and remediation guidance.

## A11y note
aria-controls is technically valid even when the target doesn't exist YET (the spec allows forward references), but most screen readers (NVDA, JAWS) report broken refs as "no target" and skip them. Pre-rendering a placeholder is the robust fix.

## Diff summary
- Files touched:
  - `crates/caco-web/static/index.html` -- added pre-rendered placeholder.
  - `crates/caco-web/static/app.js` -- dual-path lazy-populate.
  - `crates/caco-web/src/tests.rs` -- new regression test (~95 lines).
  - `.cacophony/agent/ms-dev-cacophony-caco-web/summary/pending/web/*`.
- Net pass: 547 -> 548; 0 failures.

## Operator-takeaway
38 cycles, 81 wins. Pattern (x) confirmed generalizable across 3 distinct audit shapes (CSS vars, keyframes, ARIA idrefs). **First real bug fix** discovered via pattern (x) audit (bd-91616b found 2 dead vars; bd-a753e2 found 0; bd-cf15b7 found 1 broken aria-controls — actual a11y impact). Catalog: 21 entries (a-q + s + t + u + v + w + x).
