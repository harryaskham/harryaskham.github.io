# Session summary — bd-eb81be: Home-treatment SVG mockups + IA-alternatives fork

## Goal
Contribute to Harry's broad-explore mockup review per the coordinated android-lane division: land my 4 Home-layout SVG treatments (for msd-1's rendered package) and add the IA-alternatives fork-pointer to the canonical doc (deferring the rendered side-by-side to msd-1's package, no duplication).

## Bead(s)
- bd-eb81be (UX-revamp design spike) — candidate-exploration contribution.
- Still holding bd-bdbec9 (P1 pico-streaming verify, storm-gated; unchanged).

## Before state
Harry approved producing MANY mock candidates + screenshots/mockups for review (broad-explore, not a narrow pick). The android lane self-organized: msd-1 owns the 3-candidate rendered package (A=my calm-cockpit, B=msd-1 pico-hero, C=msd-0 urgency/5-tab-hub) + trade-off table; I own the canonical doc + the IA-alternatives fork. The doc had no IA-alternatives fork; no Home-treatment mockup asset existed (image-gen proxy down per bd-7af53e).

## After state
Docs/mockups only:
- companion/android/docs/mockups/home-treatments.svg — 4 cross-cutting Home-layout SVG mockups in the Nord palette + exact hex (T1 card-feed, T2 dense-list/5-tab, T3 pico-hero, T4 pulse-chart + AttentionBadge). Hand-authored SVG (actual vector image; renders on GitHub/in browsers; portable font stack so headless renderers show text). These are Home-content options applicable WITHIN any IA — orthogonal to the A/B/C IA candidates (relabeled T1-T4 to avoid label collision). msd-1 will lift them into the package as a "Home layout options" addendum.
- ux-revamp-2026.md §13 IA alternatives — the one real fork for Harry: Option A (4 tabs + center FAB + hidden swipe-in drawer) vs Option C (5 tabs + visible grouped/searchable hub), with the calm-minimal-vs-breadth-visible trade-off; B (pico-hero) as a third take pairing with either IA; AttentionBadge as the shared converged element. Defers the rendered comparison + detailed table to msd-1's package.

## Diff summary
Landed on main — see reintegration receipt. Docs/mockups: mockups/home-treatments.svg (new) + ux-revamp-2026.md (+§13). No app code.

## Embedded artefacts
- 4 Home-layout SVG mockups (rendered + verified legible via chromium at 2x). The IA-alternatives fork framing in the canonical doc.

## Operator-takeaway
Per the coordinated division (msd-1 owns the rendered A/B/C candidate package, I own the canonical doc + fork framing), I contributed four cross-cutting Home-layout SVG mockups and the IA-alternatives decision-fork in the doc. Harry now gets the real either/or — calm-minimal (4-tab+drawer) vs breadth-visible (5-tab+hub) — framed cleanly, with msd-1's package providing the side-by-side pixel renders + trade-off table. No app code touched; fully reversible.
