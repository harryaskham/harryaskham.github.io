# Session summary — bd-eb81be: Android companion UX revamp design spike

## Goal
Operator-directed (Harry): spend a spike redesigning the Android companion app — produce mockups + an IA/visual proposal FOR REVIEW, with no app code change yet (full UX revamp).

## Bead(s)
- bd-eb81be — Android companion UX revamp design spike (created + claimed). Deliverable = design docs for review; implementation deferred to post-review follow-up beads.
- (also holding bd-bdbec9 — P1 pico-streaming on-device verify, storm-gated on ms-dev sub-5; unchanged.)

## Before state
The Android companion is a dense, parity-complete power tool: 48 UI surfaces, 2723-LOC MainActivity, primary nav Status/Chat/Agents/Beads/More with ~30 surfaces nested under a "More" junk drawer. Strong Nord identity (Polar Night / Frost / Aurora) but flat breadth, no glance/needs-you home, density without hierarchy, key loops 3-4 taps deep, drifting component vocabulary.

## After state
Two design-only docs landed under companion/android/docs/ (NO app code touched):
- ux-revamp-2026.md — current-state audit; north star ("calm glanceable cockpit"); proposed IA (4 thumb-reachable loops Home/Talk/Agents/Work + center context FAB + searchable All-surfaces drawer replacing "More"); visual-language evolution (keep Nord, add hierarchy via 3dp accent rails + 3 elevation tiers + unified StatusPill + priority typography); screen-by-screen redesign (Home, Talk, Agents, Agent Detail, Work, Settings/Connection); one component kit (SurfaceScaffold/PriorityRow/StatusPill/SectionHeader/ActionBar/AllSurfacesDrawer/PulseHeader); phased no-big-bang rollout (Phase 0 kit -> Home+nav -> Talk merge -> re-skin -> motion), each independently shippable, test-gated, reversible, no protocol change; open questions for review.
- mockups/wireframes.md — 7 ASCII wireframes (Home needs-you cockpit, Talk, Agents, Agent Detail with FFI streaming, Work board, Settings connection-mode-first, All-surfaces drawer).

## Diff summary
Landed on main — see reintegration receipt. 2 new files: companion/android/docs/ux-revamp-2026.md + companion/android/docs/mockups/wireframes.md. Docs-only; no Kotlin/Rust change (gate is Rust-only; no gradle needed).

## Embedded artefacts
- The two design docs are the review object. AI image mockups were attempted via caco image generate but the daemon image proxy is misconfigured (image_generate_failed: "relative URL without a base") — filed separately; ASCII wireframes stand in.

## Operator-takeaway
A reviewable full UX-revamp proposal for the Android companion: it keeps every capability and the Nord identity but trades the 48-peer-screen control panel for a calm cockpit — a prioritized needs-you Home, 4 thumb-reachable loops + a context FAB, one searchable surface index, and a single component kit that makes all 48 screens feel like one app. Ships in reversible, test-gated phases with no daemon/protocol change. No app code changed (per the brief). Awaiting Harry's direction pick (5 open questions in the doc) before filing implementation beads.
