# Session summary — bd-d36ac9 split-pane open no longer creates phantom tab

## Goal

Fix a TUI split-pane bug where opening the first item in a freshly created split could inherit stale visible content from the previously focused tile, causing a duplicate/phantom tab to appear instead of a clean single-pane open.

## Bead(s)

- `bd-d36ac9` — Fix duplicate tab appearing when opening new tab in split pane

## Before state

- The TUI already swapped per-tile `tab_state` after creating a new split (`bd-19682e`), so the new tile got a clean tab state.
- But the split paths did **not** also sync `content_override` to the newly focused tile.
- `current_content_pane()` prefers `content_override` before the focused tile’s actual content.
- Result: after splitting away from an agent pane, the new empty tile could still “see” the old tile’s agent as current content, making `open_nav_selection_in_focused_tile()` think the fresh split was already occupied and seed a bogus tab before opening the requested target.

## After state

- Main-workspace split paths now sync `content_override` alongside `switch_pane_view_state(...)` whenever a new focused tile is created.
- That includes:
  - keyboard split handlers
  - bulk selected-agent split opening
  - context-menu `open_in_tab` for agents
  - context-menu `open_in_tab` for beads
- Added a regression covering the bead’s real shape: split to a fresh tile, then open a different agent, and assert no phantom tab is created.
- Existing split-tab-state regressions still pass, confirming the fix layers on top of the earlier `tab_state` fix rather than replacing it.

## Diff summary

- Commit: `eb0668179` — `bd-d36ac9: sync split tab state with content override`
- Files touched:
  - `crates/caco-tui/src/app.rs`
- Tests added/validated:
  - `app::tests::opening_first_agent_in_fresh_split_does_not_create_phantom_tab_bd_d36ac9`
  - `app::tests::split_resets_tab_state_for_new_tile`
  - `app::tests::vertical_split_resets_tab_state_for_new_tile`
- Validation:
  - `cargo test -p caco-tui app::tests::opening_first_agent_in_fresh_split_does_not_create_phantom_tab_bd_d36ac9 -- --exact --nocapture`
  - `cargo test -p caco-tui app::tests::split_resets_tab_state_for_new_tile -- --exact --nocapture`
  - `cargo test -p caco-tui app::tests::vertical_split_resets_tab_state_for_new_tile -- --exact --nocapture`
  - `cargo build -p caco-tui`
  - `cargo clippy -p caco-tui --all-targets --no-deps -- -D warnings`

## Operator-takeaway

This bug was a stale-state mismatch between two pieces of split-pane state, not a generic tab-system failure. The earlier per-tile `tab_state` fix was necessary but not sufficient; the missing piece was keeping `content_override` in sync with the newly focused split so the first open into that tile starts truly empty.