# Session summary — project-health self-improvement: Operational Lessons profile section

## Goal

Per Harry's guidance ("heavy context = a good time to self-improve, make notes +
draft beads + profile fixes, then /compact to continue"), distill the operational
lessons from the long 2026-06-22/23 incident session (an ACA Attic-cache retirement
outage that broke all darwin/companion Nix builds, plus a wave of broken-on-main
failures during the echo-disabled gate window) into durable form: a scratch note,
an additive section in the project-health profile, and friction draft beads — so
future project-health runs are sharper.

## Bead(s)

- No implementation bead (operator self-improvement directive). Filed reflect-session
  P3 drafts for the friction observed (gate-coverage + 503 indeterminate-write).
- Context beads from the session: bd-30304e (darwin E0425 broken-on-main I caught
  + drove to a verified fix), bd-ad6992 (the ACA-cache outage I filed + tracked),
  bd-cc88ed (macOS smoke drift, resolved).

## Before state

- Failing tests: none owned by me; tonight's broken-on-main wave (bd-30304e darwin,
  speech_popup, caco-web, bd-38bab0) all resolved by their owners by session end.
- project-health profile had no distilled operational-lessons section.
- Lessons (darwin-cfg verification, gh-api verify, test-small gating triage,
  specialist-offer discipline, compound-gate verification, 503 write-persisted)
  lived only in this session's context.

## After state

- Failing tests: none (gate green, releases green v1.2.1342-1347, darwin restored).
- `.cacophony/profiles/project-health.md`: +1 "Operational Lessons (CI-health
  backstop)" section (additive). profile-lifecycle-audit clean (83 profiles, no
  conflicts); whitespace clean.
- Scratch note `project-health-operational-lessons` holds the full detail.

## Diff summary

- Code/content commits: the profile-section commit on this agent branch (final
  landed squash SHA from the reintegration receipt).
- Files touched: `.cacophony/profiles/project-health.md` (one additive section).
- Tests: +0 / -0 (markdown profile guidance; validated via profile-lifecycle-audit).
- Behavioural delta: future project-health agents inherit the distilled backstop
  lessons; no runtime/code behaviour change.

## Operator-takeaway

The echo-disabled gate window produced a multi-broken-on-main night; the highest-
leverage project-health catch was bd-30304e — a darwin-only `#[cfg(target_os=
"linux")]` compile break that the linux gate + linux release legs structurally
cannot see, which I only caught by verifying the darwin failure's actual error
class instead of assuming ms-mac-transit connectivity. That "verify causes even in
expected-failure windows" rule, plus the test-small gating-vs-non-gating triage
(caco-cli/caco-daemon excluded) and gh-api true-main verification, are now in the
profile. The durable systemic fix is re-enabling the reint gate (bd-ff92cd /
bd-d818db).
