# Session summary — caco-web profile self-improvement: bank hard-won operational lessons

## Goal

Per Harry's directive ("heavy context = lots of useful context; self-improve, make
notes, draft beads, profile fixes, then /compact"), durably capture this session's
hard-won caco-web operational lessons into the caco-web profile so future caco-web
specialist sessions don't re-derive them.

## Bead(s)

- (self-improvement; no implementation bead) — caco-web profile lessons capture
- Filed drafts: `bd-7b89f5` (controller no-claim-but-working reassignment race),
  `bd-57adf3` (caco-web live-DOM tooling DX) — reflect-session friction.

## Before state

- `.cacophony/profiles/caco-web.md` had no operational-lessons section; the
  session's learnings (validation discipline, fleet-blocking-fix coordination,
  durability-verify method, live-DOM tooling) lived only in my context.

## After state

- Added a "Hard-Won Operational Lessons (self-improvement)" section to
  `.cacophony/profiles/caco-web.md` with concrete, actionable guidance:
  full caco-web lib suite before every land (caco-web IS in the test-small gate);
  rebase+grep before diagnosing a broken-on-main; claim+ping-ctrl+ETA on
  fleet-blockers (heads-down != idle); gh-api compare for durability verify (not
  local merge-base / https fetch); live-DOM validation tooling (dev-server +
  nix-chromium @playwright/cli + pico mock-inject); stay-in-lane/route-don't-grab.

## Diff summary

- Code/content commit: pending final squash SHA from the reintegration receipt.
- Files touched: `.cacophony/profiles/caco-web.md` (+~55 lines, one new section).
- Tests: none (profile/docs-only; not gated by test-small).
- Behavioural delta: future caco-web specialist sessions inherit the lessons.

## Operator-takeaway

Heavy context is an asset: this session's 13 caco-web lands + the fleet-blocking
gate-restore produced concrete operational lessons (the test-small gate includes
caco-web; gh-api beats local merge-base for verify; heads-down != idle on
fleet-blockers). Banked into the profile so they compound across sessions rather
than evaporating at /compact.
