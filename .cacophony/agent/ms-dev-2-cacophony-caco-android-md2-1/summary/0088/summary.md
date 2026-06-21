# Session summary — hub doc Phase-0 groundwork record (bd-eb81be follow-up)

## Goal
Keep the single canonical Android UX-revamp hub doc current as coordination evolves during the wait for Harry's IA pick. Record the candidate-independent Phase-0 groundwork msd-0 is taking, resolving a §11-vs-§12 classification inconsistency for the AttentionBadge.

## Bead(s)
- bd-eb81be (Android companion UX-revamp design spike) follow-up — hub doc maintenance, not a new bead. msd-0 proposed candidate-independent pre-work (AttentionBadge component + Nord token system) during the IA-pick wait; I endorsed with guardrails (unwired, IA-agnostic, aesthetic-swappable) and recorded it.

## Before / After
- **Before:** §11 classified AttentionBadge as "direction-gated" while §12/§13 treated it as shared-across-all-candidates (common, not part of the fork) — a latent inconsistency. The Phase-0 candidate-independent set listed only PriorityRow (landed).
- **After:** §11 Status records the Phase-0 groundwork update: the candidate-independent set is now PriorityRow (landed) + AttentionBadge component (msd-0, the §12 three-way-converged shared pill — only its top-bar placement waits on the pick, not the component) + an aesthetic-swappable Nord token system (msd-0, formalizing the existing Nord baseline without committing to a specific aesthetic since the aesthetic axis stays unlocked). Both stay UNWIRED (no shipped UX ahead of the pick).

## Diff summary
- `companion/android/docs/ux-revamp-2026.md`: §11 Status line — appended the Phase-0 groundwork update (AttentionBadge component + Nord token system as candidate-independent unwired pre-work, with the placement-waits-not-the-component clarification + the aesthetic-swappable guardrail).

## Embedded artefacts
- Docs-only change (no gradle build; no daemon-Rust/cross-crate, so no workspace check required).

## Operator-takeaway
The hub doc now accurately records what unwired Phase-0 groundwork is safe to build during the IA-pick wait (PriorityRow landed; AttentionBadge component + Nord token system in-flight by msd-0), and resolves the AttentionBadge classification: the shared component is candidate-independent, only its placement is gated. Nothing ships into a screen ahead of Harry's pick.
