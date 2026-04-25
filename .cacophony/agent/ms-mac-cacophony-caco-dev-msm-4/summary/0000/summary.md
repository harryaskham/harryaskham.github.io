# Session summary — bd-ecdc53 macOS project-scoped dashboard mode

## Goal
Add a visible, persistent project selector to the native macOS dashboard and remove the most important hardcoded `cacophony` project assumptions from project-scoped panes.

## Bead(s)

- `bd-ecdc53` — [macOS excellence] Project-scoped dashboard mode

## Before state

- `DaemonState.refresh()` hardcoded `cacophony` for project inbox/chat, build/test/release queues, scratchpads, and source tree refresh.
- Messages compose and Operations queue actions hardcoded `cacophony`.
- Workspace had its own local project picker that was not shared with the rest of the dashboard.

## After state

- `DaemonState` owns a persisted `selectedProject` using `macos.dashboard.selectedProject`, defaulting to `cacophony`.
- Root sidebar shows a dashboard project picker populated from project status data.
- Messages compose, Operations actions, Workspace picker/timeline selection, scratchpad/source refresh, and project chat/inbox refresh now use the selected dashboard project.

## Diff summary

- Commit: `b2914496e` after stale-branch replay.
- Files touched: `companion/macos/Sources/Cacophony/App/DaemonState.swift`, `companion/macos/Sources/Cacophony/Views/RootView.swift`, `companion/macos/Sources/Cacophony/Views/MessagesPane.swift`, `companion/macos/Sources/Cacophony/Views/OperationsPane.swift`, `companion/macos/Sources/Cacophony/Views/WorkspacePane.swift`.
- Tests: no dedicated XCTest; native smoke build validates Swift compile/runtime sample path.
- Validation: `just macos-app-test`; `./docs/validate-pages.sh`.
- Behavioural delta: the macOS dashboard can visibly scope key project panes/actions to a selected project instead of always targeting `cacophony`.

## Operator-takeaway

Project selection is now a first-class dashboard state in the macOS app, visible in the sidebar and used by Messages, Operations, and Workspace surfaces.
