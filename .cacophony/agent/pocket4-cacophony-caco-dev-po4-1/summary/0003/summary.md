# Session summary — TUI Events bubble text wrapping

## Goal

The TUI Events timeline view rendered each event as a bubble with a
single-line body that hard-truncated with `…` whenever the summary
exceeded `inner_width - 4` bytes. Long coalesced event lines and
URLs silently lost trailing content. Switch the bubble body to a
display-width-aware soft-wrap so long messages stay readable inside
their bubbles instead of clipping.

## Bead(s)

- `bd-30de06` — Fix TUI Events view text wrapping in bubbles (P2 bug)

## Before state

- `crates/caco-tui/src/views/events.rs` body line did
  `format!("{body:.body_width-1}…", …)` style truncation.
- `entry_height` was a fixed `4` (connector + border + header + body)
  with `bubble_height = 3u16.min(remaining)` for graphics.
- Header/body padding used `s.content.len()` (byte length), so wide
  characters (emoji, CJK) misaligned the right border.
- No unit tests on the wrap behaviour.

## After state

- `wrap_to_width()` helper at the top of `events.rs` computes
  wrapped strings using `unicode-width`'s char width: whitespace
  boundaries preferred, oversize tokens hard-break, empty input
  returns a single empty line, zero-width defensive passthrough.
- Render loop precomputes `Vec<Vec<String>>` of body lines per
  entry; `entry_heights` and `entry_starts` tables drive both the
  scroll math (selected entry's first row) and the bubble graphics
  rect height.
- Per-entry bubble graphics rect height now matches its actual cell
  footprint (`1 + 1 + body_lines.len()`), not a fixed 3.
- Header and body padding switched to `display_width()` so wide
  characters align correctly with the right border.
- 6 new unit tests in `views::events::tests` cover: short text,
  empty input, whitespace wrap, oversize-token hard break,
  zero-width defensive passthrough, wide-char (emoji) cell-counted
  wrap. All pass; `cargo test -p caco-tui --lib views::events`
  green.

## Diff summary

- Commits: `29fdcf79a`
- Files touched: `crates/caco-tui/src/views/events.rs` (+247 / -68).
- Tests: +6 (`views::events::tests::wrap_*`).
- Behavioural delta: long event summaries now wrap to multiple lines
  inside their bubble instead of being truncated with `…`. Bubble
  graphics rects grow to match. Scroll math tracks the selected
  entry's first row even when prior bubbles wrap deeply.

## Operator-takeaway

The Events view used a "fixed-height bubble + truncate body"
shortcut that made the renderer simple but threw away signal the
moment a message was wider than the column. The variable-height
rewrite is ~2x the LOC but the renderer now honours the contract
the bead asks for — bubbles grow to fit their content. The same
`wrap_to_width()` + per-entry-start-row pattern is reusable for
any other TUI view that wants variable-height list rows
(notifications, beads with long descriptions, agents with multi-
line status). Worth keeping as a private helper for now;
graduate to `views::common::wrap_to_width()` if a second view
needs it.
