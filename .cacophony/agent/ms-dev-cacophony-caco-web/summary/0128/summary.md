# Session summary — bd-91616b: prune orphan CSS custom properties + add regression test (pivot from dedup)

## Goal
After 35 cycles of CSS dup-block audit (bd-f479b0 → bd-debc7c) reaching saturation, pivot to backlog item: prune dead CSS variables.

## Bead
- `bd-91616b`

## Audit method
1. Extract LHS of every custom-property declaration in style.css `:root` block.
2. For each, count `var(--name)` usages across all 20 static assets.

## Dead vars found
- `--bg-glow: rgba(136, 192, 208, 0.03)` (L63) — defined alongside `--bg-elev` / `--bg-hover` / `--bg-active` / `--bg-glass` family but **0 references**.
- `--transition-bounce: 0.5s cubic-bezier(0.68, -0.55, 0.265, 1.55)` (L120) — defined alongside `--transition` / `--transition-slow` / `--transition-spring` family but **0 references**.

## Kept
- `--z-base: 1` (L29) — documented as the base layer of the explicit z-index hierarchy in lines 13-29 of style.css. Preserved as design-system anchor; added to allowlist with justification.

## Fix
1. Pruned the 2 orphan declarations + added marker comments.
2. Added permanent regression test `style_css_no_orphan_custom_properties_bd_91616b` that:
   - Scans all `--name:` declarations in style.css.
   - Filters `ALLOWLIST_DOCUMENTED_DESIGN_TOKENS` (currently `--z-base`).
   - Searches `var(<name> )` / `var(<name>,)` / `var(<name>)` across 20 static assets.
   - Asserts orphan list empty with helpful failure message.
   - Defensive forward-guard against re-introduction of the 2 pruned vars.
   - Marker pin for both pruned vars.

## Pivot rationale
After 35 dedup cycles (bd-f479b0 → bd-debc7c), remaining duplicates in style.css are dominated by intentional splits or compound-tails (per pattern (m) preflight). Further dedup hits diminishing returns. Pivoted to dead-var prune.

## NEW PATTERN ENTRY (catalog #21): "x" — continuous-defense-test
Prior wins were one-shot fixes (merge a thing, add forward-guard). This win establishes **ongoing test-driven defense** that catches regressions in EVERY future commit:
- New var added without consumer → CI fails.
- Orphan var introduced via revert → CI fails.
- Cherry-pick that re-introduces pruned var → CI fails.

First instance of this pattern in the audit chain. Sibling of helpers (q) and (w), but elevated from "test-shape helper" to "audit-policy enforcement".

## Operator-visible effect
Future devs adding a new CSS var with no consumer will see:
```
bd-91616b: found N orphan CSS custom properties (declared but never
referenced via var()): ["--new-name", ...]. Either remove the
declaration, add a consumer, or add the name to
ALLOWLIST_DOCUMENTED_DESIGN_TOKENS with a justification comment.
```

## Diff summary
- Files touched:
  - `crates/caco-web/static/style.css` -- 2 orphan decls pruned + 2 marker comments.
  - `crates/caco-web/src/tests.rs` -- new regression test (~110 lines).
  - `.cacophony/agent/ms-dev-cacophony-caco-web/summary/pending/web/*`.
- Net pass: 545 -> 546; 0 failures.

## Operator-takeaway
36 cycles, 79 wins. **First continuous-defense-test pattern** (catalog entry #21 "x"); pattern catalog: 21 entries (a-q + s + t + u + v + w + x). Pivoted from CSS dedup saturation to backlog burn; orphan-var ledger now continuously enforced.
