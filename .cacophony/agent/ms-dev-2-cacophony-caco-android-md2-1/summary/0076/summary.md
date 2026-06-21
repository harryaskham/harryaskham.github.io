# Session summary — bd-eb81be Phase-0: PriorityRow (unwired groundwork)

## Goal
Build the one no-regret, direction-agnostic Phase-0 atom from the UX-revamp design spike — PriorityRow — as UNWIRED groundwork (no shipped UX change) while awaiting Harry's direction pick. Sanctioned by ctrl (direction-independent low-risk groundwork) and endorsed by md2-0 (component-kit-first, build-solo-they-review).

## Bead(s)
- bd-eb81be (UX-revamp design spike, closed) — this is the first Phase-0 implementation follow-up.
- Still holding bd-bdbec9 (P1 pico-streaming on-device verify, storm-gated; unchanged).

## Before state
The design doc's "component kit" mostly already exists (AccentCard/StatusDot/StatusBadge/SectionHeader/InfoRow/EmptyState + agentStateColor). The one genuinely-missing, fully-agnostic, non-adoption-coupled primitive — PriorityRow (the universal list atom) — did not exist.

## After state
- ui/components/PriorityRow.kt — the universal priority-row list atom. COMPOSES the existing vocabulary (AccentCard accent-wash surface + leading StatusDot, optionally live/pulsing, + a title/body/meta text stack + optional trailing slot) rather than introducing a new look, so a future adopting screen inherits current style for free. Agnostic API: title, accent, body?, meta?, live, onClick?, onLongClick?, trailing?.
- UNWIRED: no screen adopts it. The app's visible UX is unchanged — this only adds the primitive + its test. Screen adoption is deferred to a post-direction-pick phase (per ctrl: no shipped UX ahead of Harry's pick).
- PriorityRowSourceTest — 3 pins: (1) the direction-agnostic API, (2) the "compose existing AccentCard+StatusDot, don't reinvent" property, (3) the UNWIRED-groundwork invariant (asserts no screen references PriorityRow yet; the adoption phase relaxes this pin).

## Diff summary
Landed on main — see reintegration receipt. 2 new files: ui/components/PriorityRow.kt + test/PriorityRowSourceTest.kt. No existing file touched; no screen adoption.

## Embedded artefacts
- Full :app:testDebugUnitTest: 1981 tests, 0 failures+errors. PriorityRowSourceTest 3/3. :app:assembleDebug -> app-debug.apk built (validated on the ms-dev-2 build window, 5min load ~9).

## Operator-takeaway
The first concrete Phase-0 groundwork from the UX-revamp spike landed: PriorityRow, the universal list atom, built ON the existing component vocabulary and UNWIRED so the app's UX is identical to before. It's the only kit piece that's simultaneously new, fully direction-agnostic, and not adoption-coupled — so it's safe to land ahead of the direction pick; AttentionBanner/PulseHeader (direction-gated) and all screen adoption still wait on Harry's pick on the 5 open questions. A test locks the API + the unwired invariant, so adoption later is a deliberate, reviewable step.
