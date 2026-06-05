# Summary — bd-6cc7c2 non-kitty chat bubble footer pills (child of bd-179115)

## Goal
bd-6cc7c2 (child of the operator-reopened TUI graphics P1 bd-179115): chat
bubbles render incorrectly in normal (non-kitty) TUI mode. Picked up from
msm-1's compute-blocked handoff (host was saturated by a Darwin release build +
Xcode install; msm-1 mapped the full path and identified the footer pill bg
mismatch as lead #1, then reverted its diagnostic and left the tree clean).

## Root cause
`bottom_border_right_spans` builds the play (▶) / node / project / timestamp
footer segments via `bubble_pill_style` (and `bubble_project_chip`), which
hardcoded `bg = bg_elevated()` unconditionally. In text mode the rest of the
bottom border row resolves to `bg_surface()` via `style_with_bubble_bg`, so the
footer/border row rendered **two-tone** (bg_surface border fill + bg_elevated
pills) instead of a uniform bubble background — the operator-reported "incorrect"
rendering.

The opaque pill bg exists only to mask kitty border glyphs behind the pill text
under graphics (bd-358e82/bd-e76d3b); in a non-kitty terminal there is no bitmap
border to mask. The agent-name **title** pill already gated correctly via
`make_title_border_line` (`bubble_pill_style` under graphics,
`style_with_bubble_bg` in text), so only the footer path was buggy.

## Fix
`bubble_pill_style` and `bubble_project_chip` now resolve bg by graphics-active
state: `bg_elevated()` under graphics (preserving bd-358e82/bd-e76d3b),
`bg_surface()` in text mode so the footer/border row stays uniform. No title
change needed.

## Tests
- New `footer_pills_match_bubble_bg_in_text_mode_bd_6cc7c2` (text mode: every
  footer pill + the whole bottom border use bg_surface, none elevated).
- Existing `footer_chips_and_title_keep_opaque_pill_bg_under_graphics_bd_358e82_bd_e76d3b`
  still green (graphics-mode opaque pills unchanged — no regression).
- 13 footer-related caco-tui tests pass.

## Scope / remaining
Mechanism unit-verified; final **visual** confirm in a real non-kitty terminal
(footer row uniform, no two-tone pills) is operator-gated. msm-1's leads #2
(selected-bubble bg / height-area clipping) was inspected as structurally
correct and not pursued here — this slice targets the concrete footer mismatch.
Parent bd-179115 stays open until the operator visual pass across all children
(this + bd-906ae1 subpanel backgrounds + the slice-1 flicker fix) lands.

## Diff
See the landed squash commit in the reintegration receipt (code commit
`79300baf6` touching `crates/caco-tui/src/views/chat.rs`).
