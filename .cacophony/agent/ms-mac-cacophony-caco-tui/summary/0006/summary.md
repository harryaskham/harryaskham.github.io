# Session summary — Workspace-scoped merge queue in the TUI

## Goal

Make the TUI’s top-level merge-queue pane respect the active workspace tab so a project workspace can show its own isolated merge queue without clobbering the existing cluster-wide queue or the separate project-specific merge-queue pane.

## Bead(s)

- `bd-b544e8` — Add per-workspace merge queue to TUI

## Before state

- Failing tests: none in the touched TUI area, but broader validation initially hit an unrelated broken-on-main compile regression in `TopLevelBeadsConfig` test initializers (`bd-0225e7`)
- Relevant metrics: the TUI had two queue scopes only — cluster-wide (`ClusterMergeQueue`) and per-project (`ProjectMergeQueue`)
- Context: when a project workspace tab was active, the top-level merge-queue pane still read and rendered the global cluster cache, so operators could not treat it as a workspace-local queue surface

## After state

- Failing tests: none in local validation
- Relevant metrics: the TUI now maintains three distinct merge-queue caches — global, per-project, and per-workspace — with retry state isolated for each
- Context: `ClusterMergeQueue` now becomes workspace-scoped whenever `workspace_project` is set, with dedicated fetch keys, breadcrumb wording, retry handling, and render-time cache selection; the project pane remains independently scoped

## Diff summary

- Commits: `f156572f`
- Files touched: `crates/caco-tui/src/app.rs`, `crates/caco-tui/src/event.rs`, `crates/caco-tui/src/state/mod.rs`, `crates/caco-tui/src/views/merge_queue.rs`, `crates/caco-tui/src/views/tab_bar.rs`
- Tests: added targeted TUI coverage for workspace merge-queue retry, action-result cache separation, workspace breadcrumb wording, and render-path selection; validated with `cargo test -p caco-tui handle_key_r_retries_ --lib`, `cargo test -p caco-tui merge_queue_action_result_stores_workspace_cache_separately --lib`, `cargo test -p caco-tui breadcrumbs_workspace_merge_queue --lib`, `cargo test -p caco-tui render_uses_workspace_cache_when_workspace_project_is_set --lib`, `cargo test -p caco-tui merge_queue --lib`, `cargo test -p caco-tui nav --lib`, `cargo build -p caco-tui`, `cargo test-small`, and `cargo check --workspace --tests`
- Behavioural delta: the top-level merge queue now follows the active workspace tab, uses its own cache/error lifecycle, and labels itself as `Workspace > Merge Queue` in the header so it is visibly distinct from both the global cluster queue and the per-project queue pane

## Operator-takeaway

The TUI now has a real third merge-queue scope: workspace. Operators can stay inside a project workspace and use the top-level queue pane as a workspace-local view without losing the separate project-queue surface or contaminating the global queue cache.
