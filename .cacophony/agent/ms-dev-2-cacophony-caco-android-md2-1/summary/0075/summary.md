# Session summary — bd-eb81be follow-up: peer convergence + grounded Phase 0

## Goal
Incorporate the fleet's review of the Android UX-revamp design spike (md2-0 independent convergence + complementary ideas; ctrl's conditional Phase-0 green light; caco-ios-1 cross-surface parity flag) and ground the Phase-0 plan in the app's actual component inventory.

## Bead(s)
- bd-eb81be (design spike, closed) — this is a docs follow-up refinement to that landed deliverable, no re-open.
- Still holding bd-bdbec9 (P1 pico-streaming on-device verify, storm-gated on ms-dev sub-5; unchanged).

## Before state
The design doc proposed an abstract Phase-0 component kit (implicitly build-from-scratch) and a Fleet/Content/Diag/Tools/Setup drawer. It had not yet been validated against peer review or the real codebase.

## After state
Added section 11 to companion/android/docs/ux-revamp-2026.md (docs-only; no app code):
- **Independent convergence:** md2-0 ran the same spike and reached the same direction, then deferred to this doc (two surfaces, one conclusion = strong validation).
- **Complementary ideas folded in:** (a) 4-section drawer by mental model — Operate/Observe/Build/Manage (supersedes the 5-category cut); (b) global attention banner — persistent top chip, only when non-empty, on all tabs, so decision-blockers are never buried (resolves Q2: Talk can merge Chat+Inbox while Choices stay urgent-distinct).
- **Peer Q-answers (awaiting Harry's confirming pick):** 4+FAB if urgent globally surfaced; merge Chat+Inbox keep Choices distinct; ranking confirmed with critical tied at top; Phase-0 component-kit-first yes; strict Nord + optional subtle brand accent. iOS parity flag noted.
- **Grounded Phase 0 — the kit mostly EXISTS:** audit of ui/components/Components.kt shows SectionHeader, StatusDot, StatusBadge (the pill), AccentCard, InfoRow, EmptyState + agentStateColor() already ship. So Phase 0 = unify usage + fill genuine gaps (PriorityRow, SurfaceScaffold, ActionBar, AttentionBanner, PulseHeader). Only PriorityRow is new AND fully direction-agnostic AND not adoption-coupled; SurfaceScaffold/ActionBar are adoption-coupled; AttentionBanner/PulseHeader are direction-gated. Recommendation: no-regret Phase-0 build is just PriorityRow (unwired) + a unification audit; no screen adoption / no shipped UX change lands ahead of Harry's pick (per ctrl).

## Diff summary
Landed on main — see reintegration receipt. Docs-only: companion/android/docs/ux-revamp-2026.md (+section 11). No Kotlin/Rust change.

## Embedded artefacts
- The grounded Phase-0 plan: reuse existing StatusBadge/StatusDot/SectionHeader/AccentCard/InfoRow/EmptyState/agentStateColor; the one no-regret new agnostic atom is PriorityRow.

## Operator-takeaway
The UX-revamp spike is now validated by independent convergence (md2-0 reached the same direction) and grounded in the real codebase: the component "kit" mostly already exists, so Phase 0 is small (unify + add PriorityRow), not a from-scratch build. Two peer refinements improve it (mental-model drawer + a global attention banner that keeps decision-blockers visible everywhere). The 5 open questions remain Harry's product call; nothing ships into the app ahead of his direction pick. When he picks, implementation starts correctly and minimally.
