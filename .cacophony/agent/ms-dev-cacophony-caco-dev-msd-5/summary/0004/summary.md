# Session summary — bd-eeedb1 chat divider header removal

## Goal

Remove the `— previously —` and `— live —` divider header lines in the
TUI chat surface that the operator reported as visual layout breakage.

## Bead(s)

- `bd-eeedb1` — "── previously ── these headers in chat break the
  layout. remove them"

## Before state

- `build_bubble_lines` (crates/caco-tui/src/views/chat.rs) emitted a
  centered horizontal-rule divider line at the head of any hydrated
  segment (`— previously —`) and at the hydrated→live transition
  (`— live —`).
- The dividers were maintained by bd-c55c0c (intent: distinguish
  replayed history from fresh activity) and bd-ef0099 (cell-width
  tuning to keep them inside the bubble).
- Operator-visible: long box-drawing fills wrapped weirdly across
  heterogeneous terminal widths, breaking the bubble layout.

## After state

- Divider emission removed entirely from `build_bubble_lines`.
- Visual segment separation preserved structurally: at a mid-stack
  hydrated↔live transition the previous bubble is closed
  (`╰────╯`) and a fresh one is opened (`╭────╮`) instead of using
  the shared connector (`├────┤`). Hydrated bubbles still render with
  a dimmed border (bd-c55c0c) so a returning operator can still see
  the boundary.
- `bubble_stack_height` updated: leading transition (i==0) adds 0 rows;
  each mid-stack transition adds 1 extra closer row. The two formerly-
  pinning tests (`build_bubble_lines_divider_fits_within_width` and
  `bubble_stack_height_counts_dividers`) now pin the *absence* of
  divider lines and the new height arithmetic.

## Diff summary

- Files touched: `crates/caco-tui/src/views/chat.rs` only.
- Tests: 2 existing tests updated (semantics flipped from "must contain
  divider" to "must NOT contain divider"); no new tests added.
- `cargo test -p caco-tui --lib`: 2837 passed.
- `cargo test-small`: 120 passed.

## Operator-takeaway

Smallest possible change consistent with the bead text. The dimmed
border + bubble-break still mark the hydrated→live boundary, so no
information is lost; the only thing removed is the noisy horizontal
rule. If a future operator wants the rule back, the prior code is one
revert away.
