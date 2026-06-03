# Session Summary — bd-d541af (TUI graphics flicker + missing chat/nav backgrounds) + 2 broken-on-main fixes

## Goal
Operator P1 report (filed to caco-dev-po4-3 by name): TUI graphics still flicker
after recent fixes; chat bubbles render wrong in non-kitty mode; nav subpanels
render no background except one panel.

Three distinct symptoms:
1. **Flicker** — identical graphics at different screen positions flicker
   in/out (image upload IDs / virtual placements possibly mixed up).
2. **Chat bubbles** wrong in normal (non-kitty) TUI mode.
3. **Nav subpanels** render no background except one panel.

## What landed (validatable from pocket4)

### bd-d541af — symptoms 2 & 3 (missing backgrounds): FIXED
Root cause: `style_subpanel`, `style_modal`, `style_input`, `style_notification`
in `views/common.rs` gated their cell-level background on the **global**
`graphics_active()`. When a specific role's graphics surface is suppressed for a
frame (scroll/drag/resize role mask, or the all-roles `u32::MAX` resize-debounce
mask), `record_graphics_panel` records no bitmap fill — and the global check then
ALSO omitted the cell bg, leaving that panel with **no background at all**. The
"one panel that does render" is the single role whose surface wasn't suppressed.
This is the same class of bug as bd-464521 (which fixed the border + bubble
paths) but the `style_*` bg helpers were never made role-aware.

Fix: gate each helper on `graphics_active_for_role(<its role>)` instead of the
global predicate, reusing the bd-464521/bd-766acc role-aware infrastructure. When
the role's surface is suppressed, the cell-bg fallback is restored.

Test: `style_helpers_restore_bg_when_role_suppressed_bd_d541af` — asserts each
helper restores its bg under per-role suppression AND the all-roles `u32::MAX`
mask, while a non-suppressed role still omits its bg (proving role-scoped, not
all-or-nothing).

### bd-d541af — symptom 1 (flicker): NOT addressed here; delegated
Flicker is a **live-kitty-terminal-only** behavior. pocket4 is a headless tmux
node (`TERM_PROGRAM=tmux`, no `KITTY_*`, `DISPLAY=none`) and cannot reproduce or
validate a flicker fix. Investigation findings recorded for the graphics lane:
- The documented flicker mechanism — `cross_surface_retained_image_sharing`
  (kitty.rs ~L750) "byte-identical backgrounds at different screen positions can
  flicker or appear only in one placement when the same image ID is reused with
  multiple virtual placements" — is **correctly default-OFF** in production:
  `SurfaceManager::new()` sets it `false`, `apply_graphics_config` drives it from
  `config.cross_surface_image_sharing` whose default is `false`
  (`graphics_cross_surface_image_sharing_opt_in_default_off_bd_a600b8`). The
  `true` constructor (`with_capability`) is test-only.
- So there is no obvious blind code/config fix; symptom 1 needs graphics-capable
  reproduction. Filed/handed to the graphics lane (see bead notes).

### bd-028fd0 — broken-on-main (test): FIXED
`flush_graphics_requests_borrows_background_request_when_animation_unchanged_bd_5113b7`
failed on clean main (852b5c94a), independent of any agent change. It is a
source-introspection test asserting a conditional `&request` borrow that
bd-620910 removed (the flush path now ALWAYS materializes an owned
`effective = request.clone()` to clamp the background area off the last row).
In `test-small` scope → blocked the reintegration gate. Renamed to
`flush_graphics_requests_owns_background_request_with_computed_animation_bd_5113b7`
and updated assertions to pin the current always-own behavior.

### bd-f93370 — broken-on-main (clippy): FIXED
`cargo clippy -p caco-tui --lib -- -D warnings` failed on clean main with
`safe_byte_prefix is never used` (the ONLY clippy error). The helper is used only
by its own `#[cfg(test)]` module (sibling `safe_byte_slice` carries production
callers in events.rs/status.rs). Added `#[allow(dead_code)]` to preserve the
intentional caco-cli-mirror symmetry while satisfying `-D warnings`.

## Validation (queued, per shared-host policy)
- `cargo test -p caco-tui --lib` → **4104 passed, 0 failed** (full lib suite).
- `cargo clippy -p caco-tui --lib -- -D warnings` → **passed, exit 0**.
- Both broken-on-main failures confirmed pre-existing on clean origin/main
  (stash + re-run) before fixing.

## SPEC
- Background/role-aware rendering: SPEC 20.7.4 graphics config + the role-aware
  suppression contract (bd-464521/bd-766acc lineage).

## Diff
See reintegration receipt for the landed squash SHA. Agent-branch code commit
`2d11d5194`.

## Beads
- bd-d541af — symptoms 2 & 3 fixed and landing; symptom 1 (flicker) delegated to
  graphics lane (cannot validate from pocket4); bead kept open / child filed.
- bd-028fd0 — broken-on-main test fixed; close after landing.
- bd-f93370 — broken-on-main clippy fixed; close after landing.
