# Session summary — bd-5278e2: add TUI cluster timeline view

## Goal

Land the MVP TUI timeline surface as a real navigable cluster view: one new
Timeline node under Cluster, a daemon-backed fetch from `/api/v1/timeline`, and
an operator-readable pane that renders aggregated timeline entries in the same
bubble/connector style as the existing Events view.

## Bead(s)

- `bd-5278e2` — Create timeline view for TUI

## Before state

- The daemon already exposed `GET /api/v1/timeline`, but the TUI had no
  corresponding cluster Timeline node or pane.
- Operators could only see the older feed-derived Events timeline, not the new
  aggregated commit/changelog/release/bead timeline.
- No TUI state, event, or client fetch path existed for timeline data.

## After state

- Added a new `Cluster > Timeline` nav node and `ContentPane::GlobalTimeline`.
- Added client/event/state plumbing for fetching and caching the cluster
  timeline response from `/api/v1/timeline?scope=cluster`.
- Added `views/timeline.rs`, which flattens per-project timeline payloads into
  a cluster-sorted timeline and renders them using the existing bubble/
  connector visual language.
- Added workspace/tab/breadcrumb/shell-context integration so the new pane is a
  first-class TUI surface rather than a dangling enum variant.
- Added targeted tests for the new view plus nav/app regression pins.

## Diff summary

- Files touched:
  - `crates/caco-tui/src/app.rs`
  - `crates/caco-tui/src/client.rs`
  - `crates/caco-tui/src/event.rs`
  - `crates/caco-tui/src/nav.rs`
  - `crates/caco-tui/src/shell_cwd.rs`
  - `crates/caco-tui/src/state/mod.rs`
  - `crates/caco-tui/src/views/mod.rs`
  - `crates/caco-tui/src/views/nav_tree.rs`
  - `crates/caco-tui/src/views/pane_tabs.rs`
  - `crates/caco-tui/src/views/tab_bar.rs`
  - `crates/caco-tui/src/views/timeline.rs`
  - `crates/caco-tui/src/workspace.rs`
- Tests:
  - `cargo test -p caco-tui timeline -- --nocapture`
  - `cargo test -p caco-tui empty_tree -- --nocapture`
  - `cargo test -p caco-tui rebuild_nav_creates_tree -- --nocapture`
- Behavioural delta:
  - The TUI now exposes the daemon timeline pipeline directly, instead of only
    the older feed-derived Events surface.

## Operator-takeaway

This lands the MVP cluster timeline view, not the full multi-surface timeline
feature set. The important step is that the daemon timeline is now visible and
navigable in the TUI, which creates a real operator surface to iterate on for
range controls, polling policy, and richer timeline UX later.
