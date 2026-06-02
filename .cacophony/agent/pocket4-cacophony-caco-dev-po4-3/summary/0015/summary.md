# Session summary — TUI role-aware background/border fallback (bd-464521)

## Goal

Operator reported that TUI graphics still flicker and that several surfaces
render with no background: chat bubbles look broken in non-kitty rendering and
navigation subpanels show no background except one panel. This session fixed the
"missing background" class of the report by making the text-fallback decision
role-aware so suppressed panels and bubbles cannot end up with neither a bitmap
background nor a text background.

## Bead(s)

- `bd-464521` — Fix TUI graphics flicker and missing chat/navigation backgrounds (P1 bug)

## Before state

- Failing tests: none (this was a latent rendering-consistency bug, not a test failure).
- Relevant context: `record_graphics_panel`/`record_graphics_panel_with_instance`
  correctly skip suppressed roles via `graphics_active_for_role(role)` (the
  drag/scroll/resize role-suppression mask, bd-766acc / bd-01497a). But the
  paired chrome decisions used the *global* `graphics_active()`:
  - `common::graphics_block()` stripped borders to `border::EMPTY` whenever
    global graphics were active, even under full-role (`u32::MAX`) resize
    suppression where no kitty chrome is recorded.
  - chat.rs `style_with_bubble_bg` / `apply_selected_bubble_style` and the
    per-bubble `border_char` choices used global `graphics_active()`, and the
    chat message/composer panels used the non-role-aware `graphics_block()`.
  Net effect: when the `Bubble`/`Panel` roles (or all roles) were suppressed,
  the matching kitty image was never recorded yet the border/background were
  still stripped, leaving panels/bubbles with no visible chrome at all.

## After state

- Failing tests: none.
- New tests: 2 added, both passing.
  - `views::chat::tests::bubble_bg_falls_back_to_text_fill_when_bubble_role_suppressed_bd_464521`
  - `views::common::tests::graphics_block_keeps_text_border_under_full_role_suppression_bd_464521`
- Behaviour: suppressed panels/bubbles now fall back to real ratatui borders and
  `bg_surface()` text fills, consistent with the role-suppressed kitty layer.
- Validation: `cargo check -p caco-tui`, `cargo test -p caco-tui --lib bd_464521`,
  full `chat::` and `views::common::tests::` modules, and `cargo clippy -p
  caco-tui --lib` all passed via the shared-host test queue (the only clippy
  warnings are pre-existing doc-list warnings in app.rs, untouched here).

## Diff summary

- Code/content commits: pending final squash SHA from the reintegration receipt.
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA.
- Files touched: `crates/caco-tui/src/views/chat.rs`, `crates/caco-tui/src/views/common.rs`.
- Tests: +2.
- Behavioural delta: chat message/composer panels and bubble backgrounds, plus
  the unscoped `graphics_block`, now honor per-role / all-role graphics
  suppression so text fallbacks render whenever the bitmap layer is not actually
  recorded.

## Operator-takeaway

The "missing backgrounds" half of the report was a consistency gap: kitty image
recording is role-aware, but the border/background fallbacks were not, so any
role-suppressed surface (during drag/scroll/resize) lost both its bitmap and its
text chrome. That is now fixed and tested. The remaining "flicker for identical
graphics at different positions" symptom appears to be already mitigated for the
real app by bd-2b0b80 (cross-surface retained-image sharing is disabled in the
production `KittyGraphics::new()` path; only test constructors enable it), so no
further cross-placement de-dup change was made here. If flicker persists on a
live kitty/Ghostty terminal after this lands, it should be reproduced with the
isolated graphics testbed and tracked as a focused follow-up, since it needs
live-terminal validation that the headless test backend cannot provide.
