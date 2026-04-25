# Session summary — Android Beads primary navigation

## Goal

Start the operator-requested real Android surface burn-down by addressing the most obvious navigation problem: Beads are core work items and should not be hidden behind the Android More menu.

## Bead(s)

- `bd-fcfbfa` — Android companion: promote Beads to primary navigation

## Before state

- Android bottom navigation exposed Overview, Inbox, Agents, Feed, Timeline, and More.
- Beads lived only inside More, despite being a primary operator workflow for filing, browsing, and managing work.
- Timeline occupied a primary tab even though it is more of a historical/operator-inspection surface.
- Tendril CLI was available and low-resolution capture worked, but `android-cli` was not on this worker PATH and no Android simulator window was visible in the first Tendril target listing.

## After state

- Android bottom navigation now exposes Overview, Inbox, Beads, Agents, Feed, and More.
- Bead list, detail, dependency navigation, and create-bead flows now live under the primary Beads tab and preserve existing selected-project filtering.
- Timeline moved into More under Work Items as a secondary history surface.
- More no longer duplicates Beads in Quick Access or Work Items.

## Diff summary

- Commits: current `bd-fcfbfa` implementation commit
- Files touched:
  - `companion/android/app/src/main/java/com/cacophony/companion/MainActivity.kt`
- Tests:
  - `nix develop .#android --command bash -lc 'cd companion/android && gradle :app:compileDebugKotlin --no-daemon'` — passed
  - `cargo test-small` — 256 passed
  - `git diff --check` — passed
- Behavioural delta: Beads are now one tap from the Android bottom nav; Timeline is still available through More.

## Embedded artefacts

- `screenshots/display1.png` — low-resolution Tendril display capture proving the screenshot path works; an Android emulator window was not visible yet in the first target list.

## Operator-takeaway

This is the first concrete Android surface correction from the QA loop: the work-item surface is promoted to the top-level mobile navigation where operators expect it, while the Tendril screenshot path is now verified for subsequent simulator-driven iterations.
