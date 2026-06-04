# bd-6f1df3 — retire dismissed modal/popup overlay graphics (escape cleanup)

## Bead
bd-6f1df3 (P2 bug, labels graphics/kitty/modal/ui): "Fix modal escape key not
clearing kitty borders and backgrounds." When exiting a modal by pressing Escape
(like beads quickfile), the modal's kitty borders and backgrounds were not
cleaned up + redrawn, leaving visual artifacts.

## Root cause
Each modal/popup in `App::render` (crates/caco-tui/src/app.rs) renders under
`with_graphics_owner("modal:*" / "popup:*")` **only while visible**. The
kitty-side stale-surface sweeps at the end of the frame
(`retire_stale_ratatui_surfaces_for_current_redraw` /
`retire_stale_surfaces_for_current_redraw`) retire `SurfaceManager` lifecycles,
but they have **no visibility into the app-owned background bitmap cache**
(`graphics_background_surfaces` / `graphics_background_cache`, keyed
`enh:background:<owner>`). Only toasts had a dedicated retirement path
(`retire_hidden_toast_overlay_graphics`); every modal/popup lacked the
equivalent, so when a modal was dismissed (Escape, click-away, action) its
background bitmap lingered on screen until unrelated layout churn cleared it.

## Fix
- New `prev_active_overlay_owners: HashSet<String>` field tracks which overlay
  owners rendered last frame.
- New `active_overlay_owners()` returns the owners visible this frame (mirrors
  the 19 modal/popup visibility conditions in `render`; toasts excluded — they
  have their own path).
- New `retire_dismissed_overlay_graphics(active)` diffs prev vs current active
  owners and, for each dismissed owner, removes its `enh:background:<owner>`
  entries from `graphics_background_surfaces` / `graphics_background_cache`,
  retires its kitty surfaces by owner prefix, and invalidates border
  integration so its borders+backgrounds are deleted on the next upload pass.
- Wired into `render` right after the modal block, before
  `flush_graphics_requests()`, so dismissed-overlay backgrounds are removed in
  the same frame.

### Key correctness detail
The retirement must be **per-owner surgical**: it must NOT call
`clear_surface_dirty_tracking()` (a full `graphics_background_cache.clear()`),
which would wrongly drop still-open overlays' cached backgrounds. The unit test
caught exactly this in the first draft (a still-open popup's cache was wiped);
the fix uses per-owner `.retain()` only.

## Tests
- New `retire_dismissed_overlay_graphics_drops_closed_modal_background_bd_6f1df3`:
  seeds two overlays' background surface+cache, marks both active last frame,
  then dismisses one — asserts the dismissed modal's background surface + cache
  are retired while the still-open popup's are preserved, and the active-owner
  set is recorded for the next diff. Green.
- Full `cargo test -p caco-tui --lib`: exit 0 (no regression to the heavily-used
  `render()` path).
- `cargo check -p caco-tui --lib`: exit 0.

## Validation boundary
Source/logic-validated headless. Live kitty visual confirmation (open a modal,
press Escape, confirm no border/background artifacts remain) routed to msm-2
(only graphics-capable node).

## Coordination
Sibling modal bead bd-ff8c0d (P1, modal click-target/z-index input routing) is
owned by po4-2 (mouse-event dispatch / hit-test ordering) — distinct lane, no
file overlap (po4-2 in input path, this in graphics retirement).

## SPEC
SPEC 20.8 (TUI kitty graphics surface/placement lifecycle), SPEC 20.2 (overlays).
No contract change; adds overlay-dismiss retirement parity with the existing
toast path.

## Diff
See the reintegration receipt for the landed squash SHA. Slice commit on the
agent branch: bd-6f1df3 overlay-dismiss graphics retirement in
crates/caco-tui/src/app.rs (active_overlay_owners +
retire_dismissed_overlay_graphics + prev_active_overlay_owners field + unit test).
