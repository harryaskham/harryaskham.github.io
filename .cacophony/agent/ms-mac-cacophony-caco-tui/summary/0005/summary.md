# Session summary — Project merge queue pane in the TUI

## Goal

Add a project-scoped merge queue view to the TUI so operators can inspect reintegration activity within one project without dropping back to the cluster-wide queue view, and make that pane behave like the existing project tools surfaces in navigation, tabs, persistence, and refresh/retry handling.

## Bead(s)

- `bd-340338` — Add per-project merge queue to TUI

## Before state

- Failing tests: none known in the touched TUI area
- Relevant metrics: the TUI only exposed the cluster-wide `Merge Queue` pane; project tool groups had no project-scoped queue entry
- Context: `caco agent merge-queue list --json --project <project>` already existed in the CLI, but the TUI had no per-project node, no project-keyed state storage, and no persisted workspace selection for that surface

## After state

- Failing tests: none in local validation
- Relevant metrics: project tool groups now include `Merge Queue`; project queue data is stored independently from the cluster queue; project merge queue panes round-trip through workspace persistence and support `r` refresh/retry
- Context: the TUI now shells out to the existing CLI primitive with `--project <project>`, renders project-scoped queue reports in a dedicated pane, and no longer lets the global checkout-refresh `r` shortcut swallow pane-local refresh handlers

## Diff summary

- Commits: `6be296f6`
- Files touched: `crates/caco-tui/src/app.rs`, `crates/caco-tui/src/event.rs`, `crates/caco-tui/src/nav.rs`, `crates/caco-tui/src/shell_cwd.rs`, `crates/caco-tui/src/state/mod.rs`, `crates/caco-tui/src/views/merge_queue.rs`, `crates/caco-tui/src/views/nav_tree.rs`, `crates/caco-tui/src/views/pane_tabs.rs`, `crates/caco-tui/src/views/tab_bar.rs`, `crates/caco-tui/src/workspace.rs`
- Tests: +7 targeted TUI tests covering nav placement/mapping, refresh-retry behavior, and workspace persistence round-trip for the new pane
- Behavioural delta: project workspaces now expose a `Merge Queue` tool pane backed by project-filtered CLI data, with isolated cache/error state per project. While landing that work, I also fixed a real bug where a global `r` shortcut intercepted pane-local refresh/retry handlers, which previously prevented merge-queue retry from firing at all.

## Operator-takeaway

The per-project merge queue pane is now a first-class project tool in the TUI, not just a cluster-only surface. The important hidden fix is that `r` now reaches pane-local refresh/retry handlers instead of being eaten by the generic project checkout refresh path, so the new queue pane and other existing tool panes behave the way their UI hints claim.
