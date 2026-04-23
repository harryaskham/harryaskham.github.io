# Session summary — bd-78bf58 force tmux pane resize on terminal-resize + broadcast attach

## Goal

Eliminate the bd-78bf58 footgun where embedded tmux panes silently
take on dimensions set by *other* attached TUI / terminal clients.
Route the two remaining non-forced resize call sites
(terminal-resize event propagation and broadcast-attach) through
the existing forced variant so the local dedup cache cannot
suppress a real resize event when another client has changed
server-side `window-size` since our last send.

## Bead(s)

- `bd-78bf58` — Attaching an agent's tmux pane should always call the set rows/cols on the inner term to match the container precisely
- (filed alongside, unrelated: `bd-38f991` — [upstream-tendril] DSL silently demotes 'Return' to literal text payload, relayed from picasso-health roamer agent)

## Before state

- Failing tests: bd-c19193 (pre-existing, not addressed here).
- The single-attach path (`attach_tmux_for_agent`) already used `start_tmux_resize_forced` from a prior partial bd-78bf58 landing.
- The terminal-resize event handler (`Terminal(Resize) → propagate_tmux_resize_for_tiles`) called the non-forced `start_tmux_resize_for_tile`, gated by the `last_tmux_resize` dedup cache. After a sibling TUI client moved tmux's `window-size`, our cache key still matched our last send → resize was suppressed → pane stayed at the sibling's dimensions.
- The broadcast-attach path (`attach_tmux_broadcast`) had the same problem for every newly attached pane in the broadcast set.

## After state

- Failing tests: unchanged (bd-c19193 pre-existing).
- All 6 tmux-resize unit tests pass, including the new `terminal_resize_propagation_forces_tmux_resize_through_cache`.
- `propagate_tmux_resize_for_tiles` now uses the forced variant for both the per-tile path and the single-tile attached / preview branches.
- `attach_tmux_broadcast` now uses `start_tmux_resize_forced` for each visible agent.
- The bd-fbe775 dedup cache continues to suppress spurious mouse-move resize storms — the forced variant only fires from operator-driven events (terminal resize, attach), which are rare relative to the storm the cache was designed for.

## Diff summary

- Commits: `f9036e11 bd-78bf58: force tmux pane resize on terminal-resize and broadcast attach`
- Files touched: `crates/caco-tui/src/app.rs` (+86/-5)
- Tests: +1 / -0 / flipped 0
- Behavioural delta: terminal-resize and broadcast-attach now always push a resize subprocess call to tmux; user-visible result is that an embedded pane no longer "sticks" at another client's dimensions after a resize.

## Operator-takeaway

The bd-fbe775 per-agent resize cache exists to absorb the
mouse-move resize storm and is otherwise correct, but it cannot
represent server-side state set by *other* clients — only what
THIS TUI last sent. Any code path that fires in response to a
real (non-storm) signal — operator terminal resize, attach,
re-attach — must bypass the cache. After this bead, the only
remaining non-forced call sites in `app.rs` are the
storm-absorbing paths the cache was actually designed for
(post-bead-event redraw selection, PTY-spawn-failure fallback,
which already trip via other state transitions).
