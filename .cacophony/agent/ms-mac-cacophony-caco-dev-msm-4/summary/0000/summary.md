# Session summary — bd-5d6538 macOS diagnostics empty-state guidance

## Goal

Improve the native macOS Diagnostics pane so logs and performance telemetry are easier to scan, especially when data is empty or filters hide every result.

## Bead(s)

- `bd-5d6538` — [macOS excellence] Diagnostics empty-state guidance polish

## Before state

- Diagnostics had useful filter controls and metric cards, but no explicit guidance describing how to scan logs versus performance telemetry.
- Empty logs/perf lists appeared as blank list areas, so operators could not tell whether data was missing or filters were too narrow.
- Clearing filters required manually editing both filter controls.

## After state

- Added a guidance card below the filter row explaining how to scan the selected diagnostics tab and when to copy/share visible data.
- Added a clear-filters affordance when text or severity filters are active.
- Added dedicated empty states for no loaded data and no matches, with tailored recovery copy for logs and performance telemetry.
- Added a reusable `DiagnosticsEmptyState` view for icon, message, and optional action rendering.

## Diff summary

- Commit: `cf1d05242` after replay onto the remote agent branch.
- Files touched: `companion/macos/Sources/Cacophony/Views/DiagnosticsPane.swift`.
- Tests: `just macos-app-test`; `./docs/validate-pages.sh`; `git diff --check`.
- Behavioural delta: no API behavior changed; diagnostics now has explicit empty/loading and filtered-no-match guidance.

## Operator-takeaway

Diagnostics should now explain what to do when it looks blank: refresh if no data is loaded, clear filters if the current query hides everything, and copy/share the visible slice for handoff.
