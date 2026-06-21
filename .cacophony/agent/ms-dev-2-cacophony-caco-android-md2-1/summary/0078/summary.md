# Session summary — bd-eb81be follow-up: four-way convergence + folded deltas

## Goal
Consolidate the android UX-revamp into ONE canonical doc (this one) by folding the two genuine deltas from the second wave of independent spikes — msd-1's pico-as-hero and msd-0's AttentionBadge — and recording that the parallel decks are retired.

## Bead(s)
- bd-eb81be (UX-revamp design spike) — consolidation follow-up.
- Still holding bd-bdbec9 (P1 pico-streaming verify, storm-gated; unchanged).

## Before state
The doc had md2-0's convergence + complementary ideas (§11) including a generic "global attention banner". Separately, caco-android msd-0 and msd-1 had independently produced parallel UX-revamp proposals (msd-1's bd-24690d, msd-0 scratch) without finding this landed doc first; msd-0 caught the duplication, msd-1 drove dedup.

## After state
ux-revamp-2026.md updated (docs-only):
- §6.4 Agent Detail — folded msd-1's delta (1): pico/agent-conversation is the HERO, the default full-bleed surface, reached by a shared-element transition straight from an Agents card (Harry's macOS-parity goal); Terminal/Diff/Logs/Summary become secondary segments.
- §12 (new) Second-wave convergence — records FOUR-way convergence (md2-0 + msd-0 + msd-1 + me), parallel decks retired, this doc canonical. Folds msd-0's delta (2) as a concrete AttentionBadge spec: a persistent top-app-bar pill on every destination showing the highest-priority count, severity-tinted, ranked by the §3-P1 actionability order, tap-to-jump, hides when empty; the full feed stays on Home. Three independent proposals of the same global-urgency idea resolve Q2's placement toward a top-bar badge (still Harry's call).
- §11 gap list updated (AttentionBadge) + PriorityRow marked LANDED (fa99d4d8c0 + 0424d89924).

## Diff summary
Landed on main — see reintegration receipt. Docs-only: companion/android/docs/ux-revamp-2026.md (§6.4 edit + new §12 + §11 tweak). No app code.

## Embedded artefacts
- One canonical UX-revamp doc; four android agents converged; two refinements folded (pico-hero, AttentionBadge).

## Operator-takeaway
The android UX-revamp is now a single canonical doc with four independent android agents converging on the same direction — very strong validation. Two genuine refinements are folded in: pico/agent-conversation promoted to the hero surface with a direct shared-element destination (macOS-parity), and a concrete global AttentionBadge (top-app-bar urgency that follows the operator across tabs). The 5 open questions remain Harry's product call — now sharper (Q2 placement trending to a top-bar badge). Parallel decks retired; no app code touched.
