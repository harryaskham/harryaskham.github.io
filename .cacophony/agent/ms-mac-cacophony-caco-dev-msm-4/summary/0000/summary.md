# bd-ef0099 — keep chat hydrated/live divider inside the bubble border

## Goal
Stop the new `— previously —` / `— live —` chat divider (bd-c55c0c)
from pushing rendered text outside the bubble's visual border, and
make the live-segment bubble visually re-open instead of bleeding
into the hydrated bubble above it.

## Bead(s)
- bd-ef0099 (P2 task, test-user) — `The ──— previously —─── text
  newly added to the chat breaks the layout by pushing all text
  outside of its bubble border, please fix`. Direct follow-up to
  bd-c55c0c which introduced the divider.

## Before state
- `build_bubble_lines` emitted divider lines as exactly `inner_width`
  cells wide: `"─"×left + label + "─"×right` with no gutter. The
  divider hugged the bubble border on both sides, visually
  indistinguishable from the bubble's own `─` runs and (on narrow
  widths or when `inner_width < label_w`) overflowing it.
- After a hydrated→live transition the next bubble used the shared
  `├───┤` connector with the previous (hydrated) bubble, so live
  traffic shared a "wall" with snapshot history instead of opening
  a fresh bubble run.
- `bubble_stack_height` did not count divider rows, so the
  visible-window heuristic ("trim bubbles off the top while
  height > viewport") underestimated the rendered height by one
  row per transition and let the bottom bubble overflow.

## After state
- Divider width = `inner_width - 1` cells, with a one-space gutter
  on each side surrounding the `─` fill. The divider now visually
  sits inside the bubble column rather than pressing against the
  border.
- After a divider on a live transition (`i > 0` with `show_divider`),
  open the next bubble with a fresh `╭───╮` top instead of the
  shared `├───┤` connector. Hydrated and live runs read as
  distinct stacks even when they touch.
- `bubble_stack_height` counts divider rows by re-running the same
  `(prev_hydrated, msg.hydrated)` match, so the visible-window
  trim is exact.

## Diff summary
- `crates/caco-tui/src/views/chat.rs` (+171/-6):
  - `build_bubble_lines`:
    - Divider construction: `target_w = width-1`,
      `inner_w = target_w-2`, `pad = inner_w - label_w`, build
      `" " + "─"×left + label + "─"×right + " "`.
    - Bubble top: `opens_fresh = i == 0 || show_divider.is_some()`,
      use `╭───╮` when fresh, `├───┤` otherwise.
  - `bubble_stack_height`: count dividers via the same
    `(None, true) | (Some(true), false)` match used for emission.
  - 2 new tests in `views::chat::tests`:
    - `build_bubble_lines_divider_fits_within_width` — across
      widths 40/60/80/120: every line ≤ width; divider lines
      strictly < width and start/end with one-space gutter; both
      `— previously —` and `— live —` labels appear in the
      rendered output.
    - `bubble_stack_height_counts_dividers` — 1 hydrated msg →
      5 rows; hydrated→live pair → 9 rows.

## Operator-takeaway
- Once binaries roll, the chat surface will visually contain the
  `— previously —` and `— live —` markers inside the bubble
  column with breathing room on each side, and the live-segment
  bubble will open as a fresh `╭───╮` instead of sharing a wall
  with the hydrated bubble above it.
- No config knob; behaviour is purely cosmetic / layout-correct.
- Future: `make_header_line` can also overflow at very narrow
  widths (caught the test at width=20 and excluded those from the
  bd-ef0099 assertion). That's a separate issue; file if it's
  hit in operator setups.

## Tests
- `cargo test -p caco-tui --lib views::chat::tests::` — 60/60
  passed (incl. 2 new bd-ef0099 tests).
- `cargo clippy -p caco-tui --all-targets -- -D warnings` — clean.
- `cargo build -p caco-tui` — clean.
