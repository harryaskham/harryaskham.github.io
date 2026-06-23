# Session summary — caco-web filter-chip count badge contrast (a11y)

## Goal

Fix a WCAG contrast defect found by the caco-web duty-cycle contrast probe: the
inactive filter-chip count badges (e.g. "96", "17157" on Agents/Beads filter rows)
rendered at ~1.45:1 — far below AA — and were nearly invisible.

## Bead(s)

- `bd-fd0cad` — caco-web filter-chip count badges sub-AA contrast (1.45:1).
- `bd-74fc8d` (draft) — broader --text-muted secondary labels ~2.82:1, for triage.

## Before state

- Failing tests: none. `.filter-chip-count` (inactive) used `color: var(--text-faint)`
  (#434c5e) on `var(--bg-deep)` (#2e3440) = ~1.45:1 (chromium audit, #agents + #beads).
  Active counts were fine (var(--accent), ~9.3:1).

## After state

- Failing tests: none. `cargo test -p caco-web --lib` passed (gate discipline).
- `static/style.css` `.filter-chip-count`: `color: var(--text-faint)` →
  `var(--text-secondary)` (nord4 #d8dee9) = ~9.25:1 (verified). `--text-muted`
  (~3.4:1 on the deep inset bg) was insufficient. Active-state rule unchanged.

## Diff summary

- Code commit: pending final squash SHA from the reintegration receipt.
- Files: `crates/caco-web/static/style.css` (one rule).
- Tests: +0 (CSS; no `.filter-chip-count` needle test; caco-web lib tests green).
- Behavioural delta: filter-chip count badges are now legible (9.25:1 vs 1.45:1).

## Embedded artefacts

- `web/screenshots/filter-chip-count-contrast-after.png` — filter chips after fix.

## Operator-takeaway

A systematic contrast audit found the filter-chip count badges were effectively
invisible (1.45:1, --text-faint on the deep inset bg) — fixed to ~9.25:1 with the
existing --text-secondary token while keeping them visually secondary via size +
pill. Also filed a draft (bd-74fc8d) for the broader --text-muted ~2.82:1 secondary
labels, which is a coordinated design-token decision rather than a unilateral change.
