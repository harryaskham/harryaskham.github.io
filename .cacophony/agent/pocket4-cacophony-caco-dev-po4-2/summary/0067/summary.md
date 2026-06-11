# Session summary — bd-873c35 TUI terminal-input reader isolation

## Goal

Reduce attached-agent keypress loss when the TUI is busy rendering many panes by moving terminal input collection off the Tokio render/update task path and into an input-dedicated blocking thread that forwards events into the existing priority event queue.

## Bead(s)

- `bd-873c35` — caco tui key input unresponsive when lots of agent panes are visible and scrolling.
- Related but not closed here: `bd-09418b` — attached agent panes randomly detach while typing; po4-1 routed the OSC-response-injection finding as adjacent input-loop work.

## Before state

- `App::run` used `crossterm::event::EventStream` in a Tokio task to read terminal input.
- The main event loop already prioritised `TuiEvent::Terminal` within each drained batch, but terminal events still had to be produced by a Tokio task that could be delayed under render-heavy / multi-pane scrolling conditions.
- A related random-detach symptom (`bd-09418b`) suggests a separate raw terminal response filtering gap; that is recorded as adjacent and not claimed as solved by this slice.

## After state

- Terminal input is read by a dedicated OS thread named `caco-tui-terminal-events` using blocking `crossterm::event::read()`.
- The blocking reader forwards key/paste/mouse/resize events into the same `TuiEvent::Terminal` priority path, preserving existing input dispatch semantics while isolating event production from render/update Tokio scheduling delays.
- The old async `EventStream` dependency/import is removed from `app.rs`.
- Added a focused regression/source-guard test asserting the blocking-reader path is used and the old `EventStream` construction is not present.

## Diff summary

- Code/content commits: pending final squash SHA from reintegration receipt.
- Summary artefact commit: intentionally omitted; this file must not self-reference its own mutable SHA.
- Files touched:
  - `crates/caco-tui/src/app.rs`
  - `.cacophony/agent/pocket4-cacophony-caco-dev-po4-2/summary/pending/summary.md`
- Tests: added one focused TUI app test for the input-reader architecture.
- Behavioural delta: terminal events are produced by a kernel-blocking reader thread independent of the Tokio rendering workload, then fed into the existing high-priority terminal event queue.

## Operator-takeaway

This slice addresses the responsiveness side of `bd-873c35` by preventing render-heavy async work from delaying stdin event production. It does not pretend to fully solve the adjacent `bd-09418b` random detach / OSC-response-injection hypothesis; that needs raw terminal response filtering or live attached-pane validation as a separate follow-up.

## Validation

- `tj-8efa0db2` passed: `cargo test -p caco-tui --lib terminal_input_reader_uses_blocking_thread_bd_873c35 -- --test-threads=1`.
- `tj-728cc95f` passed: `cargo check -p caco-tui --lib`.
- `git diff --check` passed.
- Note: `cargo check -p caco-tui --lib` still surfaces a pre-existing `caco-daemon` warning about `was_non_terminal`; this slice did not introduce it.
