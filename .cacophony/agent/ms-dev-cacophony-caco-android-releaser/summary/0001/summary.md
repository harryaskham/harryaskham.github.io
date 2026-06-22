# Session summary — Android UX revamp design spike (bd-9e7d70)

## Goal

Operator (Harry) asked for a design spike: a full UX revamp of the Android
companion app — design only, no code, with mockups assembled for review. Produce
a clear redesign direction and reviewable hero-screen mockups, grounded in the
existing implementation, and hand it back for an approval decision.

## Bead(s)

- `bd-9e7d70` — Android companion full UX revamp — design spike (mockups for review, no code)
- (also shipped this session: Play rollouts vc 15984 and 16054 via the cadence loop)

## Before state

- Failing tests: none (design-only).
- Current Android design language is the "gradient polish" system (bd-1c0bdd,
  GRADIENT_POLISH_AUDIT.md): Nord palette, Material 3, but decorative gradient
  brushes on nearly every surface (HeroHeader, AccentCard, EmptyState tiles,
  GradientIconTile, GradientFab, GradientStatCard, SectionHeader bars). Overview
  stacks two hero headers. Visually busy; no consolidated redesign proposal existed.

## After state

- Failing tests: none.
- New design artifacts under companion/android/design/:
  - UX_REVAMP_SPIKE.md — redesign thesis (gradient-polish → calm Material 3),
    7 design principles, current→proposed per hero surface (Overview/Chat/Beads/
    Agents/More), proposed implementation slices (post-approval), and 3 operator
    questions.
  - mockups/mockups.html + mockups/mockups.png — hand-authored HTML/CSS mockups
    (exact Nord hex, real M3 components/text) rendered via chromium headless to a
    3480x1880 sheet of 4 hero screens; verified by vision-model description.
- No app/code changes; implementation deliberately deferred to follow-up beads.

## Diff summary

- Code/content commit: pending final squash SHA from reintegration receipt
- Summary artefact commit: intentionally omitted (no self-reference)
- Files touched: companion/android/design/UX_REVAMP_SPIKE.md (new),
  companion/android/design/mockups/mockups.html (new),
  companion/android/design/mockups/mockups.png (new)
- Tests: +0 / -0 / flipped 0 (design-only)
- Behavioural delta: none in app; adds a reviewable redesign proposal + mockups.

## Embedded artefacts

- mockups/mockups.png — 4 redesigned hero screens (Overview · Chat · Beads · Agents)
- mockups/mockups.html — reproducible mockup source

## Operator-takeaway

The revamp keeps the Nord brand and the existing accessibility/type/spacing
foundations but spends the decoration budget far more sparingly: one hero per
screen, tonal cards instead of per-card gradients, integrated status accents.
Direction + mockups are ready; it is now an approval/decision point (Nord-default
+ Material You opt-in? how bold? which surface first?) before any implementation
beads are opened.
