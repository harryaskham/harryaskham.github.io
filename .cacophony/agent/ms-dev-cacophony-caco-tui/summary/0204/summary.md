# Session summary — caco-tui chat touch swipe/drag-to-scroll (bd-3eba75)

## Goal

During the operator's overnight burndown push, take a genuinely-implementable
caco-tui bead from a headless text node. Harry reported on an Android/Termux
touch terminal that he cannot scroll the TUI chat (pico) view by finger
swipe/drag, though tmux panes in the same terminal do. Add drag-motion scroll to
the chat view so touch-terminal operators can swipe chat history like a tmux
pane — implemented and unit-tested headlessly, with the touch *feel* left for
Harry's live device verification.

## Bead(s)

- `bd-3eba75` — [operator-flagged] TUI chat (pico) view: no swipe/drag-to-scroll like tmux panes on Termux touch terminal
- Reflection draft filed: `bd-ac6e57` — current_content_pane() derives from nav selection in single-tile mode (test gotcha)
- (Triaged this session, not landed: routed offline-agent daemon beads to caco-ctrl; added a render-perf characterization to the P0 `bd-c0ebb6` which is graphics-perf-owner/live-verify bound.)

## Before state

- Failing tests: none.
- crates/caco-tui/src/app.rs: `EnableMouseCapture` already enables crossterm drag/any-motion reporting (modes 1002/1003), so SGR drag events arrive, but the app mouse handler's `Drag(MouseButton::Left)` arm only handled resize-drag (sidebar/tile/bottom-panel) + source-tree drag-selection. Chat scroll existed only on the mouse-wheel path (`ScrollUp`/`ScrollDown` -> `handle_content_scroll_up/down`, bd-82948b). Finger swipes (drag-motion) were ignored by the chat view.

## After state

- Failing tests: none. Focused queued run `cargo test -p caco-tui chat_touch_drag_swipe_scrolls_history_bd_3eba75` = 1 passed (real multi-crate compile + test name in the job log, per the false-GREEN caution; ran on ms-dev, not the corrupted ms-mac target).
- A plain left-drag (no resize `drag_state`) over a chat pane now translates vertical row delta into chat scrolls via the existing `handle_content_scroll_up/down` helpers — finger-up swipe scrolls back through history, mirroring the wheel path. Scoped strictly to chat panes (GlobalChat/ProjectChat/ProjectGroupChat/NodeChat) so it never clashes with source-tree drag-selection or other drag behaviors; tracked via a new `content_swipe_last_row`, cleared on mouse-up.

## Diff summary

- Code/content commit: `80d50bf8cf` (final landed squash SHA from the reintegration receipt).
- Files touched: `crates/caco-tui/src/app.rs` (new `content_swipe_last_row` field + init; chat-swipe handling in the `Drag(Left)` no-resize arm; clear on `Up(Left)`; new test + setup helper).
- Tests: +1 (`chat_touch_drag_swipe_scrolls_history_bd_3eba75`); 0 removed; 0 flipped.
- Behavioural delta: touch/drag swipe over a chat pane scrolls chat history (new); no change to wheel scroll, resize-drag, source-tree drag-selection, or non-chat panes.

## Operator-takeaway

Chat now supports touch swipe-to-scroll on Termux-style terminals, scoped to chat
panes so nothing else changes. The drag->scroll LOGIC is unit-tested, but the
touch FEEL (direction and one-line-per-row sensitivity) needs Harry's eye on the
actual device — if it feels inverted or too fast/slow, those are one-liners
(flip the delta sign / divide the step count). Reflection draft bd-ac6e57 captures
a real test-writing gotcha: caco-tui single-tile `current_content_pane()` resolves
via nav selection, not `set_focused_content` — content-gated app mouse tests must
set `nav.selected`.
