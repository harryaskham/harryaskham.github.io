# Session summary — Android summaries row accessibility

## Goal

Continue Android summaries polish by making list rows more useful to screen-reader users before they open a summary detail.

## Bead(s)

- `bd-667679` — Android summaries: improve list row accessibility labels
- related: `bd-a5e2fa` — Session-summary viewers across TUI, caco-web, and Android

## Before state

- Android summaries rows were touchable cards with visible index, title, agent/project chips, bead chips, and artefact badges.
- Accessibility semantics relied mostly on the nested visible text and generic clickability.
- A screen-reader user could miss important context such as artefact availability or the row's open-detail action.

## After state

- Each summary row now computes a dedicated accessibility label.
- The row label includes summary index, title, short agent, project, bead-reference count, artefact presence, and the open-detail instruction.
- Rows advertise `Role.Button` through Compose semantics.
- The implementation keeps the existing visual hierarchy unchanged.

## Diff summary

- Commits: current `bd-667679` implementation commit
- Files touched:
  - `companion/android/app/src/main/java/com/cacophony/companion/ui/summaries/SummariesScreen.kt`
- Tests:
  - `nix develop .#android --command bash -lc 'cd companion/android && gradle :app:compileDebugKotlin --no-daemon'` — passed
  - `cargo test-small` — 256 passed
- Behavioural delta: Android TalkBack-style navigation gets richer per-row context without changing the visible UI.

## Operator-takeaway

Android summaries are now more accessible: list rows announce what they represent and what artefacts they contain before the operator opens the detail screen.
