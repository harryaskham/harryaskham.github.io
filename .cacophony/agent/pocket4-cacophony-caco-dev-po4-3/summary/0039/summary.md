# Summary — bd-906ae1 nav subpanel background z-lift (child of bd-179115)

## Goal
bd-906ae1 (child of the operator-reopened TUI graphics P1 bd-179115): nav
subpanels render no background **except one panel**. Land a fix so all nested
nav subpanels render their gradient backgrounds.

## Root cause
Every base-tier `enh:background:*` surface resolves to the single
`z_layers::BACKGROUND` z (-120) via `surface_z_index` (key-based, no nesting
input). The outer sidebar registers a **full-area** gradient background
(`record_graphics_panel("sidebar:navigation", area, PanelRole::Sidebar, ...)`),
and every nested subpanel registers its own background at `inner.x` **inside**
that area (`record_graphics_panel_with_visibility(..., PanelRole::SidebarPanel,
..., nesting)`). All of them land at the SAME z, so the parent's full-area fill
and the nested fills overlap ambiguously — only one composites visibly.

A diagnostic test confirmed each subpanel DOES produce a background surface
through `render_graphics_background` (so generation/keying is correct); the
collapse is purely a z-compositing problem.

## Fix
Add a per-surface background nesting z-offset on `SurfaceManager`
(`background_nesting_z_offset: HashMap<String, i32>`):
- `set_background_nesting_offset(key, nesting_level)` lifts deeper fills by depth,
  clamped to `MAX_BACKGROUND_NESTING_Z_OFFSET = 9` so a lifted background stays
  strictly below `BORDER_CHROME` (-110) (parent -120, level-1 -119, ...).
- Consulted in `effective_z_index` before the cell-bg fold so relative ordering
  is preserved; cleaned up on surface remove/clear.
- Wired at background registration in app.rs, **scoped to Sidebar/SidebarPanel
  roles** to bound the blast radius; other nested backgrounds keep existing z.

## Tests
- `distinct_sidebar_subpanels_each_render_a_background_surface_bd_906ae1`
  (diagnostic: two distinct subpanels each render a surface).
- `nested_background_z_offset_lifts_subpanel_above_parent_bd_906ae1`
  (parent < level-1 < level-2, all < BORDER_CHROME; clamp; clear restores base).
- Full caco-tui `kitty::` suite green; no z-index regression.

## Scope / remaining
Mechanism is unit-verified; the final **visual** (all nested nav subpanels
showing gradient backgrounds at a real kitty terminal) needs operator confirm.
bd-906ae1 stays in_progress pending that visual confirm. Sibling bd-6cc7c2
(non-kitty chat bubbles) is owned by msm-1. Parent bd-179115 stays open until
both children + the operator visual pass land.

## Diff
See the landed squash commit in the reintegration receipt (code commit
`60f0c7948` touching `crates/caco-tui/src/app.rs` + `crates/caco-tui/src/kitty.rs`).
