# Session summary — bd-eb81be: full-flow combinations + SET3-is-shipped note

## Goal
Per ctrl's add-depth (don't-hold) guidance while awaiting Harry's pick: render mix-and-match full-flow combinations showing cohesive "complete looks", and fold in md2-0's verification that the embedded-showcase candidate is real shipped capability.

## Bead(s)
- bd-eb81be (UX-revamp design spike) — depth-add.
- Still holding bd-bdbec9 (P1 pico-streaming verify, storm-gated; unchanged).

## Before state
The candidate set covered 6 screens axis-by-axis (IA / aesthetic / per-screen layout) but didn't show integrated combinations, so Harry had to mentally compose a cohesive look. Also, SET3 embedded-showcase had not been confirmed against shipped reality.

## After state
- companion/android/docs/mockups/full-flow-combinations.svg — 2 cohesive end-to-end directions side by side, rendered + verified via chromium:
  - Combo A — Calm cockpit + Nord-M3 (dark, current-identity refined): Home needs-you cockpit + Agents grouped cards (least-change, familiar).
  - Combo B — Conversation-first + Material-You (light, bolder): pico-hero Home + pico-full-bleed Agent Detail, in a light Material-You / Pixel-like identity (dynamic, modern).
  The dark-Nord vs light-Material-You contrast makes the aesthetic axis tangible.
- Hub (§13) updated: references the full-flow SVG, and records md2-0's verification that SET3 embedded-showcase is **real shipped capability** (in-app web dashboard + agent-PTY terminal; bd-06bcee + bd-0116b1 both closed) — de-risks picking it.

## Diff summary
Landed on main — see reintegration receipt. Docs/mockups: mockups/full-flow-combinations.svg (new) + ux-revamp-2026.md (refs + SET3-shipped note). No app code.

## Embedded artefacts
- 2 cohesive full-flow combination mockups; SET3-is-shipped verification (md2-0) folded into the hub.

## Operator-takeaway
Added depth to the candidate set: two mix-and-match full-flow combinations show Harry cohesive complete looks (a least-change Nord-dark calm-cockpit vs a bolder conversation-first Material-You-light direction), making the dark-vs-light aesthetic and the calm-vs-conversation IA choices tangible rather than axis-by-axis. The doc also now records that the embedded-showcase candidate is real shipped capability (verified by the implementer), not vaporware — de-risking that option. All design/mockups only, reversible, nothing wired into a screen. Awaiting Harry's pick.
