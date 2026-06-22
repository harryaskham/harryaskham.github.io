# Session summary — bd-a753e2: @keyframes orphan-defense regression test (pattern x extension)

## Goal
Apply pattern (x) continuous-defense-test (catalog #21, established in bd-91616b) to a second audit category: orphan `@keyframes` definitions.

## Bead
- `bd-a753e2`

## Audit results
- 70 `@keyframes` declared across 5 CSS files (style.css ×66, summaries.css/workspace-a11y.css/workspace-chat-pane.css/workspace-stt-mic.css ×1 each).
- 0 currently orphan — **clean baseline; no prune needed**.

## Fix
Added regression test as continuous defense against future bloat.

`style_css_no_orphan_keyframes_bd_a753e2`:
- Aggregates `@keyframes <name> { }` blocks across 13 .css files.
- **Token-bounded substring match** (prevents `slide` from matching `slideIn`).
- Bundle of 28 static assets searched for `animation:` and `animation-name:` value contents.
- Conservative value extraction (bounded by `;` `}` newline) prevents matching across rule boundaries.
- `ALLOWLIST_RESERVED_KEYFRAMES` placeholder (currently empty) for future intentional reservations.
- Failure message guides remediation per bd-91616b precedent.

## Pattern (x) generalization
**First confirmed extension** of pattern (x). Demonstrates that the pattern is generalizable to multiple audit categories:
1. CSS custom properties (bd-91616b — pruned 2 + defended).
2. `@keyframes` (bd-a753e2 — 0 prune + defended).
3. Future targets: JS exports / ARIA targets / named media queries / z-index hierarchy entries / theme tokens / etc.

Each pattern (x) extension follows the same shape:
- Aggregate **definitions** across declarative space.
- Aggregate **consumers** across consumer space.
- **Subtract**: orphans = defs − consumers − allowlist.
- **Assert** orphans empty with helpful failure message.
- Optional: **forward-guard** against re-introduction of any pruned members.

## Diff summary
- Files touched:
  - `crates/caco-web/src/tests.rs` -- new regression test (~145 lines).
  - `.cacophony/agent/ms-dev-cacophony-caco-web/summary/pending/web/*`.
- Net pass: 546 -> 547; 0 failures.

## Operator-takeaway
37 cycles, 80 wins. Pattern (x) generalized: 2 of 5+ planned audit categories complete. Test infrastructure now defends against orphan accumulation in two declarative spaces continuously. Catalog: 21 entries (a-q + s + t + u + v + w + x).
