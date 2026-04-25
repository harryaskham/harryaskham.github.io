# Fix multi-tile click leak in agent detail inner tab bar (bd-5e26ce)

## Goal

Operator: "CACO agent detail tabs in the TUI often become non-navigable;
clicks don't switch tabs reliably." Root-cause and fix the multi-tile
click-routing bug so all tabs in the focused tile are consistently
clickable.

## Bead(s)

- `bd-5e26ce` — Fix CACO agent detail tabs navigation in TUI.

## Before state

`crates/caco-tui/src/views/agent_detail.rs::render` unconditionally
wrote inner-tab geometry into shared `TuiState`:

```rust
state.agent_detail_inner_tab_click_ranges = inner_tab_result.click_ranges;
state.agent_detail_inner_tab_variants     = inner_tab_result.tab_variants;
state.agent_detail_inner_tab_row          = inner_tab_result.tab_row;
state.agent_diff_sidebar_rect             = inner_tab_result.diff_sidebar_rect;
state.agent_diff_content_rect             = inner_tab_result.diff_content_rect;
```

In a multi-tile workspace where two agent-detail panes render in the
same frame, the LAST-rendered pane wins. Mouse hit-testing in
`app.rs` then resolves clicks on tile A's tab strip against tile B's
column ranges + row index → either no match (tab unresponsive) or
wrong tab activated. Exact symptom in bd-5e26ce.

## After state

`render` and `render_persistent` take a new `is_focused: bool`
parameter. Inner-tab geometry is persisted only when the call is
focused, so non-focused agent-detail tiles draw the same UI but
leave shared hit-testing state untouched. Single-tile workspaces are
unaffected (the only tile is always focused).

Doc-comment on `render` records the rationale so a future refactor
that drops the gate also drops the bug.

## Diff summary

- `crates/caco-tui/src/views/agent_detail.rs`:
  - `pub fn render` and `pub fn render_persistent` gain
    `is_focused: bool`.
  - The 5-line geometry writeback is wrapped in `if is_focused`.
  - 47 in-test invocations updated to pass `true` (single-tile
    test scenes are always "focused"). Comment cites bd-5e26ce.
  - 2 new tests:
    - `render_focused_tile_persists_inner_tab_geometry` — verifies
      a focused render writes click_ranges, variants, and a non-zero
      tab_row.
    - `render_non_focused_tile_does_not_clobber_geometry` — seeds
      sentinel geometry, runs a non-focused render, asserts all
      three sentinels survive byte-for-byte.
- `crates/caco-tui/src/app.rs`: 3 call sites pass through the
  existing `is_focused: bool` already in scope from
  `render_content_in_area`.

## Operator-takeaway

In multi-tile layouts, agent-detail tab strips on non-focused tiles
no longer poison the focused tile's click hit-testing. Click on any
tab in any focused tile reliably switches that tile's inner tab.
Single-tile use unchanged.
