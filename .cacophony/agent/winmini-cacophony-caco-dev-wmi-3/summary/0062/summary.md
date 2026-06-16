# Session summary — pico agent nudge delivery to the correct target (bd-32c18c)

## Goal

Fix pico managed agents receiving nudge/injection text on the wrong tmux target.
Pico agents run the headless picophony host in their tmux pane and read a
per-agent Unix socket (`<agent_dir>/pico.sock`), not pane keystrokes — so any
nudge sent via `tmux send-keys` lands in the pico host pane (the "socket
manager"), not the agent session. The interactive nudge / `caco msg
--direct-send` path already routed pico over the socket (bd-019da7), but several
timer/lifecycle-driven nudge paths still called `tmux_send_*` unconditionally.

## Bead(s)

- `bd-32c18c` — Fix pico agents nudge text delivery to correct tmux pane
  (agents/messaging/pico/tmux; P1 bug).

## Before state

- `AgentManager::nudge` (bd-019da7) and the `caco msg --direct-send` / broadcast
  / remote `/nudge` paths correctly route pico → host socket.
- But four daemon-driven nudge/injection paths in
  `crates/caco-daemon/src/agent/lifecycle.rs` sent raw `tmux_send_keys_on` /
  `tmux_send_text_on` to the agent's tmux session for ALL agent types,
  mis-delivering to the pico host pane:
  1. watchdog nudges (bd-3c3de4),
  2. timed profile nudges (bd-3cb880),
  3. heartbeat inbox injection (bd-b8cd9b),
  4. unpause continue nudge (bd-9ba494).
- Failing tests: none.

## After state

- New module-level helper `deliver_pico_nudge_if_pico(agent_type, agent_dir,
  message, context, agent_id) -> bool`: for pico agents it delivers over the
  host socket (`pico_session::deliver_nudge`, a `followUp` prompt) and returns
  true (success or logged failure — no tmux fallback, mirroring
  `AgentManager::nudge`); for non-pico it returns false so the caller keeps its
  existing tmux delivery.
- All four sites now route pico via the helper before falling back to tmux. The
  watchdog and timed nudge structs carry `agent_type` + `agent_dir`. The
  heartbeat-inbox and unpause paths derive them in place. The unpause site (under
  the lifecycle lock) uses a bounded 5s background task so socket I/O never
  blocks the lock.
- Three new unit tests (`bd_32c18c_pico_nudge_routing_tests`): non-pico returns
  false; pico routes to a live socket with the correct `Prompt{followUp}` wire
  form; pico with a missing socket is still handled (no tmux fallback).
- Failing tests: none. Out-of-band validation green:
  `cargo test -p caco-daemon --lib bd_32c18c` (tj-706c5652) and
  `cargo clippy -p caco-daemon --lib` (tj-857cf74b); rustfmt-clean on touched
  lines. The reintegration gate runs the full workspace check/test-small/clippy.

## Diff summary

- Code/content commit: pending final squash SHA from reintegration receipt.
- `crates/caco-daemon/src/agent/lifecycle.rs`: +helper + `bd_32c18c_*` test
  module; `WatchdogNudge`/`TimedNudge` gain `agent_type` + `agent_dir`; four
  nudge dispatch sites route pico to the host socket.
- Tests: +3 unit tests.
- Behavioural delta: pico agents now receive watchdog / timed / heartbeat-inbox /
  unpause nudges as structured socket follow-ups (waking/queuing politely)
  instead of tmux keystrokes to the host pane. Non-pico (pi/claude/codex)
  delivery is unchanged.

## Operator-takeaway

Pico agents read their host socket, not tmux pane keystrokes — so every place the
daemon delivers a nudge/injection must route pico over the socket. This session
closed the remaining four timer/lifecycle paths that still used `tmux send-keys`,
behind a single shared helper, so a pico agent's watchdog/timed/heartbeat/unpause
nudges now reach its session instead of the host "socket-manager" pane. Non-pico
agents are untouched. If new daemon-side nudge paths are added later, route them
through `deliver_pico_nudge_if_pico` rather than calling `tmux_send_*` directly.
