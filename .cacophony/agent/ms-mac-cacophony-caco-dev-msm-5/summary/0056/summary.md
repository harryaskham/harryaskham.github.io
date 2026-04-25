# Session summary — Android summaries detail retry

## Goal

Continue Android summaries recovery polish by making failed detail loads recoverable in place without forcing the operator back to the list.

## Bead(s)

- `bd-1df952` — Android summaries: add detail load retry action
- related: `bd-a5e2fa` — Session-summary viewers across TUI, caco-web, and Android

## Before state

- Android summaries detail loading could fail into a static empty state.
- The selected summary context was preserved, but there was no visible retry action in the failed-detail state.
- Operators had to navigate away or rely on broader refresh flows to try the selected detail again.

## After state

- The summary screen tracks a detail retry nonce and includes it in the selected-detail fetch effect.
- Failed detail loads now show a centered `TRY AGAIN` action.
- Retrying re-fetches only the selected summary detail while preserving the selected row context.

## Diff summary

- Commits: current `bd-1df952` implementation commit
- Files touched:
  - `companion/android/app/src/main/java/com/cacophony/companion/ui/summaries/SummariesScreen.kt`
- Tests:
  - `nix develop .#android --command bash -lc 'cd companion/android && gradle :app:compileDebugKotlin --no-daemon'` — passed
  - `cargo test-small` — 256 passed
- Behavioural delta: Android summaries detail errors now have an in-place retry affordance.

## Operator-takeaway

Android summaries now match the recovery standard established on web: if a selected summary detail fails to load, the operator can retry the detail directly without losing context.
