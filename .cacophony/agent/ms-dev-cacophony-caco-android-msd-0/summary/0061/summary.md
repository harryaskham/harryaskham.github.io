# Revert parallel redesign spike — fold into canonical AURORA hub (bd-304b4e reconciliation)

## Goal
Remove the parallel mockups/redesign/ spike I landed during the ms-dev WSL2 downtime, which duplicated the android crew's canonical AURORA redesign (AuroraTokens.kt + live Compose heroes + ux-revamp-2026.md sec15-17 + mockups/stunning/). Coordinated with md2-1: single hub, no fragmentation.

## Changes
Removed companion/android/docs/mockups/redesign/ entirely (4 hero mocks + DESIGN-SYSTEM.md + showcase). The canonical mockups/ + ux-revamp hub already cover these. HTML sources retained privately in /tmp as layout inspiration for the follow-on canonical Compose work.

## Follow-on (post-compact, re-scoped bd-304b4e)
Build the Work/Beads board LIVE hero canonically: AuroraBeadsHero.kt on AuroraTokens.kt, matching AuroraHomeHero.kt, folded into AuroraPreviewScreen.kt, ux-revamp sec16/17 updated. md2-1 owns Agent-detail (bd-1336c3); no overlap.

## Diff
Single docs/asset removal; see reintegration receipt for the landed SHA.
