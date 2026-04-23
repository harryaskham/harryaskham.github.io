# Session summary — GH Pages vs webapp visual audit (bd-90d4d3)

## Goal
Document every visual divergence between `docs/` (GitHub Pages) and
`crates/caco-web/static/` (webapp) so the parallel docs-refresh beads
have a single source of truth for the gap list.

## Bead(s)
- bd-90d4d3 — Audit current GitHub Pages design against webapp visual design
- siblings (parallel-claimable, blocked on this audit):
  bd-0e2372 (colors/typo), bd-0ffc0d (layout), bd-c2d025 (validation)

## Before state
- Failing tests: none.
- No written record of how docs/ diverges from caco-web/static/. The
  parallel docs-refresh beads (bd-0e2372 colors/typo, bd-0ffc0d layout,
  bd-c2d025 validation) had no shared gap-inventory to consume; each
  would have re-discovered the same deltas in isolation.

## After state
- `docs/audits/bd-90d4d3-github-pages-vs-webapp-design.md` (~190 lines)
  with 9 sections covering palette, typography, spacing/radii/shadows,
  layout, components, accents, icons/logo, a11y/motion, and
  prioritised recommendations.
- 8 concrete deltas tabulated; 6 ordered follow-up actions documented
  for the sibling beads to pull from.

## Diff summary
- New: `docs/audits/bd-90d4d3-github-pages-vs-webapp-design.md`
- No code changes, no test changes (audit-only bead).

## Operator-takeaway
The webapp has built a full design system (~7878 lines of CSS, 13
design-system var-tokens, 5-step shadow ramp, 3 named transitions) on
top of the same Nord palette docs uses. docs (~291 lines) has stayed
at v0.1 of that system and now feels visibly behind. Cheapest wins
the sibling beads can take from this audit:
  1. shared `_tokens.css` partial (single source of truth)
  2. font-stack swap to Inter + JetBrains Mono via the same
     Google Fonts link the webapp uses
  3. backfill four a11y gaps (color-scheme, reduced-motion,
     :focus-visible, skip-link)
  4. logo + favicon parity
