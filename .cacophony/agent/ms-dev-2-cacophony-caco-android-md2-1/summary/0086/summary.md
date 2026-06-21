# Session summary — bd-eb81be: §14 cites durable aesthetic-mockup paths

## Goal
Update the hub's §14 to cite the now-durable aesthetic mockup paths after the releaser landed them on main (b66da6d4c7), resolving the bd-be6d54 replication gap so the aesthetic axis is GitHub-viewable everywhere.

## Bead(s)
- bd-eb81be (UX-revamp design spike) — hub durability follow-up.
- Still holding bd-bdbec9 (P1 pico-streaming verify, storm-gated; unchanged).

## Before state
§14 (visual aesthetic candidates) cited only the file-cache artifact IDs, which were not fetchable from ms-dev-2 (the bd-be6d54 replication gap) — with a caveat to fetch from a working node.

## After state
The releaser landed the aesthetic mockups durably on main under companion/android/design/mockups/. §14 now cites the durable, GitHub-viewable paths: mockups.png (Nord-M3, Overview/Chat/Beads/Agents), candidate-b-controlroom.png (Control-room), candidate-c-materialyou.png (Material-You), plus their .html sources and UX_REVAMP_SPIKE.md. Verified the Nord-M3 overview render (polished Material-3 + Nord-dark, high-fidelity HTML/CSS->chromium). The replication-gap caveat is resolved; file-cache IDs retained for in-app viewing.

## Diff summary
Landed on main — see reintegration receipt. Docs-only: ux-revamp-2026.md §14 (durable paths). No app code.

## Embedded artefacts
- Aesthetic axis now durably referenced in the hub (3 landed PNGs + sources + proposal).

## Operator-takeaway
The aesthetic axis (Nord-M3 / Control-room / Material-You) is now durably landed on main and GitHub-viewable, and the hub's §14 cites those openable paths instead of the non-fetchable file-cache IDs — the bd-be6d54 replication gap no longer affects Harry's ability to review the aesthetic candidates. The whole candidate set (IA + aesthetic + per-screen layouts + full-flow combos) is now durably viewable in-repo. Awaiting Harry's pick.
