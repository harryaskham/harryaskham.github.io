# bd-ff8c0d — Fix modal click target priority over agent pane

## Bead
bd-ff8c0d (P1 bug, profile=dev, labels: click-handling, modal, ui, z-index).

## Problem
When a modal dialog rendered on top of an agent pane, clicking the modal
incorrectly focused/attached the BACKGROUND agent pane instead of interacting
with the modal. `handle_modal_mouse` (caco-tui/src/app.rs) runs before
`forward_mouse_to_attached` and is supposed to consume mouse events for any
on-top modal — but five modal overlays were missing from its guard list, so
their clicks fell through to the attached-pane forwarder.

## Root cause
`handle_modal_mouse` covered most modals (spawn, teleport, bead create/edit,
help, confirm, context menu, fuzzy picker, speech popup, views menu, etc.) but
NOT these on-top overlays:
- config_error_modal
- quick_file_bead_dialog
- workspace_picker_visible
- scratchpad_picker_visible
- scratchpad_dispatch_visible

When any of these was open and clicked over an attached tmux/shell/SSH pane,
the function returned false → the click reached forward_mouse_to_attached →
the background pane got focused/attached. Same click-through class as the
spawn-dialog fix (bd-72484d / bd-60448b).

## Fix
Added a single consuming guard in `handle_modal_mouse`, right after the
daemon_update_modal block (so it takes priority before any pane forwarding),
returning `true` for all five overlays. They are keyboard-driven (Enter/Esc/
arrows), so consuming all mouse events keeps each modal the active click target
for every region it occupies — satisfying all three acceptance criteria:
- click on a visible modal registers on the modal, not the pane behind it
- the agent pane does not become focused/attached
- the modal remains the active click target for all regions it occupies

## Test
Added `modal_mouse_guard_consumes_clicks_for_overlay_modals_bd_ff8c0d`
(source-inspection style, matching existing bd-44f7fc click tests): slices the
`handle_modal_mouse` body and asserts each of the five modal fields is covered
and that the bd-ff8c0d guard block returns true (blocks click-through).

## Validation (queued, shared-host policy)
- cargo test -p caco-tui --lib modal_mouse_guard_..._bd_ff8c0d → 1 passed, Exit 0
- cargo test -p caco-tui --lib (full suite) → passed, Exit 0
- cargo clippy -p caco-tui -- -D warnings → Exit 0

## Files
- crates/caco-tui/src/app.rs (guard in handle_modal_mouse + regression test)

## Scope note
Distinct from bd-6f1df3 (modal escape not clearing kitty borders) which is the
graphics/kitty lane and needs live-compositor validation — left for graphics-owner.
