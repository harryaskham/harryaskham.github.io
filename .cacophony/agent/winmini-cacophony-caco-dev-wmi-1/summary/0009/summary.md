# Session summary — bd-ec4fdc per-project TUI timeline

## Goal

Continue the oldest-first timeline burndown by turning the already-landed cluster timeline work into a real per-project TUI pane instead of leaving project-scoped timeline browsing stuck behind the global-only view.

## Bead(s)

- `bd-ec4fdc` — Implement per-project timeline view in TUI

## Before state

- Failing tests: none in scope before this change; the cluster timeline pane from `bd-5278e2` was already green.
- Relevant metrics: the TUI exposed `NavNode::GlobalTimeline` / `ContentPane::GlobalTimeline`, but project sections only had `ProjectEvents` and no `ProjectTimeline` node or pane.
- Context: `/api/v1/timeline?scope=cluster` already returned per-project timelines inside the shared response, so the missing piece was TUI wiring and renderer reuse rather than a new daemon API.

## After state

- Failing tests: none observed in the targeted TUI validation.
- Relevant metrics: project roots now expose a `Timeline` child, the TUI workspace persistence layer understands `ProjectTimeline`, and the shared timeline renderer can render either the cluster view or a project-filtered subset from the same cached response.
- Context: opening a project timeline now lazily reuses the existing cluster fetch, filters events to the requested project, and renders project-specific breadcrumbs, tabs, nav styling, and list counts.

## Diff summary

- Commits: `030c9efe1`
- Files touched: `crates/caco-tui/src/app.rs`, `crates/caco-tui/src/nav.rs`, `crates/caco-tui/src/shell_cwd.rs`, `crates/caco-tui/src/views/nav_tree.rs`, `crates/caco-tui/src/views/pane_tabs.rs`, `crates/caco-tui/src/views/tab_bar.rs`, `crates/caco-tui/src/views/timeline.rs`, `crates/caco-tui/src/workspace.rs`
- Tests: `cargo test -p caco-tui rebuild_nav_creates_tree -- --nocapture`; `cargo test -p caco-tui empty_tree -- --nocapture`; `cargo test -p caco-tui timeline -- --nocapture`
- Behavioural delta: the TUI now supports both cluster and per-project aggregated timeline panes, with the project pane sharing the same timeline cache and bubble renderer while filtering entries and labels to the selected project.

## Operator-takeaway

This turned out to be the honest small child bead that the earlier note predicted: no new daemon endpoint was needed, just proper TUI plumbing on top of the already-landed cluster timeline response.
