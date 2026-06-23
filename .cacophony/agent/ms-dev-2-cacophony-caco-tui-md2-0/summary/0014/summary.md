# Session summary — caco-tui profile self-improvement (2026-06-22 burndown lessons)

## Goal

Per Harry's directive (when context is heavy, self-improve + profile fixes + /compact), encode the hard-won lessons from a high-churn burndown night into the caco-tui profile so future caco-tui agents avoid the same gate-re-blocks + dup-claims.

## Bead(s)

- No bead (operator-directed self-improvement). Related reflect draft: bd-b41716 (dup-claim coordination friction).

## Before state

- caco-tui profile Validation section recommended a FOCUSED test filter (`cargo test -p caco-tui $TUI_TEST_FILTER`), which misses runtime assertion drift in other tests — the class that re-blocked the cacophony-fast-tests gate 3x tonight (speech_popup, caco-web).

## After state

- Added a "Validation + coordination lessons" subsection: run FULL caco-tui lib tests before landing; test-small gating scope (excludes caco-cli/caco-daemon/caco/caco-sidecar); gh-api verify (not HTTPS fetch); avoid dup-claims (check sibling/claim-state).

## Diff summary

- Files touched: `.cacophony/profiles/caco-tui.md` (profile docs, +~35 lines).
- Docs/profile-only change (no Rust) → skips the reint gate (bd-45114c).

## Operator-takeaway

Tonight's three gate-re-blocks all traced to stale UI test-assertion drift landing via the echo-disabled gate; the profile now tells caco-tui agents to run the full crate tests before UI lands (not just a focused filter), plus the test-small gating scope, gh-api verify method, and dup-claim avoidance — concrete, tested lessons captured durably rather than lost with the session context.
