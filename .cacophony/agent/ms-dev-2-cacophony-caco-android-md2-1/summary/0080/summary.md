# Session summary — bd-eb81be: Candidate A render sanity-check + §13 concrete fork-pointer

## Goal
Sanity-check Candidate A's rendered mockup against my canonical spec (msd-1's explicit ask before Harry relies on it) and make the doc's IA-alternatives fork-pointer cite the now-landed package files.

## Bead(s)
- bd-eb81be (UX-revamp design spike) — render-verification + doc follow-up.
- Still holding bd-bdbec9 (P1 pico-streaming verify, storm-gated; unchanged).

## Before state
msd-1 rendered + posted the 3-candidate package (A/B/C) to Harry but the renders weren't reachable on my node (file-cache replication gap to ms-dev-2; pull 0 / id-not-found / commit absent on origin/cacophony-state). msd-1 then landed the PNGs on main (a2bceec9a2) under companion/android/docs/mockups/. My doc §13 referenced "msd-1's package" abstractly; A was not yet pixel-verified.

## After state
- Fetched the renders via direct GitHub fetch (local origin/main was the stale daemon mirror) and viewed them. **Candidate A verified faithful** to ux-revamp-2026.md §4 + §6.1: needs-you cockpit Home (priority order choice>DM>failed-build>waiting-agent>FYI), accent-rail priority cards, 4 tabs (Home/Talk/Agents/Work) + center FAB, Talk unifies chat+DMs+choices, and the hidden swipe-in categorized All-surfaces drawer ("not a permanent tab") as the correct A-vs-C contrast. Comparison sheet (ABC) reads clean — same Nord, "compare IA not styling", shared Choices badge. One minor optional note flagged: A's drawer uses my older §4 cut (Fleet/Content/Diag/Tools/Setup) vs the §11 Operate/Observe/Build/Manage refinement (either fine).
- Confirmed A faithful to Harry + msd-1; spoke a brief review-ready note to Harry.
- ux-revamp-2026.md §13 updated to cite the concrete landed package files (android-ux-comparison-ABC.png + per-candidate renders + android-ux-3candidate-package.md) and record A as sanity-checked faithful.

## Diff summary
Landed on main — see reintegration receipt. Docs-only: ux-revamp-2026.md §13 (concrete package citations + A-verified note). No app code.

## Embedded artefacts
- A's render verified faithful; the 3-candidate comparison package (msd-1's, on main) is Harry-ready. Flagged the file-cache replication gap to ms-dev-2 (daemon lane).

## Operator-takeaway
The 3-candidate Android UX mockup package (A calm-cockpit, B conversation-first/pico-hero, C urgency-forward/5-tab-hub) is rendered, landed on main, and verified — Candidate A faithfully reflects the canonical spec I own. The doc's IA-alternatives fork now cites the concrete renders. Everything is design/mockups only, reversible, awaiting Harry's pick (or mix) on the real IA fork: calm-minimal (4-tab+hidden-drawer) vs breadth-visible (5-tab+hub), with B pairing on either and the AttentionBadge shared.
