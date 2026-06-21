# Session summary — bd-eb81be: Agents + Agent-Detail layout candidates (2nd mockup set)

## Goal
Per ctrl's produce-don't-hold correction (Harry's broad-explore IS the work — generate the candidate set he picks FROM), deliver a 2nd distinct mockup candidate set for comparison, beyond Home/IA/aesthetic.

## Bead(s)
- bd-eb81be (UX-revamp design spike) — candidate generation.
- Still holding bd-bdbec9 (P1 pico-streaming verify, storm-gated; unchanged).

## Before state
The candidate set had Home-layout treatments (T1-T4), IA candidates (A/B/C), and aesthetic candidates (Nord-M3/Control-room/Material-You) — but no per-screen layout candidates for the other high-traffic screens.

## After state
- companion/android/docs/mockups/agents-detail-candidates.svg — 5 cross-cutting layout treatments, hand-authored SVG rendered + verified via chromium (Nord palette, exact hex, portable fonts):
  - **Agents screen:** AG1 grouped node cards (today's contract refined), AG2 dense list (max density), AG3 status grid (at-a-glance fleet tiles colored by state).
  - **Agent Detail:** AD1 segmented tabs (identity header + Replies/Terminal/Diff/Logs + action bar), AD2 pico-hero full-bleed (conversation fills the screen, controls overlaid — B-aligned).
- Referenced in the hub (ux-revamp-2026.md §13) alongside home-treatments.svg as cross-cutting layout options orthogonal to the IA + aesthetic axes.

## Diff summary
Landed on main — see reintegration receipt. Docs/mockups: mockups/agents-detail-candidates.svg (new) + ux-revamp-2026.md (hub ref). No app code.

## Embedded artefacts
- 5 Agents + Agent-Detail layout mockups (rendered, legible). Hub now indexes Home + Agents + Agent-Detail layout treatments across the candidate axes.

## Operator-takeaway
Delivered a 2nd mockup candidate set for Harry's broad-explore review: five Agents + Agent-Detail layout treatments (grouped-cards / dense-list / status-grid; segmented-tabs / pico-hero-full-bleed) in Nord, orthogonal to the IA and aesthetic axes so they mix-and-match. The single hub now spans Home, Agents, and Agent-Detail layout options plus the IA and aesthetic axes. More per-screen sets (Work, Talk) next. All design/mockups only, reversible, no UX shipped.
