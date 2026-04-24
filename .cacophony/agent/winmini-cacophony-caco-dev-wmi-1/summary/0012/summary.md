# Session summary — bd-be13be TUI dirtiness marker

## Goal

Burn down the next contained ready TUI bead by surfacing git dirtiness directly in the sidebar so operators can spot agents and persistent controllers with unreintegrated local edits at a glance.

## Bead(s)

- `bd-be13be` — Add git dirtiness indicator to agent/persistent status in TUI sidebar

## Before state

- Failing tests: none in scope before this change, but the sidebar had no way to distinguish clean checkouts from dirty ones.
- Relevant metrics: daemon snapshot rows already carried checkout paths, but neither `ui_stream::AgentSnapshot` nor `replication::SnapshotAgentRow` exposed a dirtiness bit, and the TUI nav brief structs had no dirtiness-aware icon path.
- Context: operators could only discover dirty work by drilling into agent details or shelling into the checkout. Persistent rows were especially opaque because they only showed state, node, and declaration name.

## After state

- Failing tests: none observed in the targeted daemon/TUI validation.
- Relevant metrics: agent snapshots and replicated peer rows now carry `git_dirty`, the TUI state tracks that flag, and nav agent/persistent icons gain a `✱` suffix when their backing checkout is dirty.
- Context: the TUI sidebar now shows the dirtiness marker inline with agent and persistent status icons without changing the existing status wording or tree structure.

## Diff summary

- Commits: `e53dab5dd`
- Files touched: `crates/caco-daemon/src/{beads.rs,lib.rs,replication.rs,ui_stream.rs}`, `crates/caco-daemon/tests/{daemon.rs,multinode.rs}`, `crates/caco-tui/src/{app.rs,nav.rs,shell_cwd.rs,shell_tile_lane.rs,state/mod.rs}`, `crates/caco-tui/src/app/benchmark_support.rs`, `crates/caco-tui/src/state/tests.rs`, `crates/caco-tui/src/views/{agent_detail.rs,chat.rs,fuzzy_picker.rs,project_tree.rs}`
- Tests: `cargo test -p caco-daemon agent_snapshot_with_checkout_path_round_trips -- --nocapture`; `cargo test -p caco-tui dirty_nav_icons_append_marker_bd_be13be -- --nocapture`; `cargo test -p caco-tui empty_tree -- --nocapture`; `cargo test -p caco-tui rebuild_nav_creates_tree -- --nocapture`
- Behavioural delta: operators now see a dirtiness mark beside sidebar status icons for dirty agent and persistent checkouts, including replicated peer agents carried through daemon snapshots.

## Operator-takeaway

This stayed an honest burndown slice by reusing the daemon’s existing git dirtiness probe and threading a single boolean through snapshot/replication into the nav renderer, rather than inventing a second icon system or a separate details-only surface.
