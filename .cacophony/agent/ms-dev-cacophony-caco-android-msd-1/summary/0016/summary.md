# Summary — Android UX-revamp 3-candidate mockups landed for review (bd-24690d)

## What
Landed the rendered Android UX-revamp review package under companion/android/docs/mockups/: the A/B/C side-by-side comparison sheet, the 3 individual candidate renders (A calm-cockpit, B conversation-first/pico-hero, C urgency-forward), and the trade-off table doc. Design only, no app code.

## Why
Harry requested multiple mock candidates for review. Three android agents independently re-derived UX-revamp directions; converged (dedup) on md2-1's landed ux-revamp-2026.md as canonical + 3 distinct candidates. File-cache replication to ms-dev-2 failed (propagation gap, flagged to ctrl), so landed the mocks to main for durable team+Harry review, consistent with the existing wireframes.md + home-treatments.svg there.

## Diff
See the reintegration receipt for the landed squash SHA. Adds 4 PNG mockups + 1 markdown trade-off table under companion/android/docs/mockups/.

## Status
Awaiting Harry's direction pick (pico-hero vs glance-cockpit; 4-tab+hidden-drawer vs 5-tab+visible-hub; FAB; light mode) and md2-1's A-render sanity-check. No implementation until greenlight.
