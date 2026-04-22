# bd-c69e23 — TUI: can no longer type to an agent while they are emitting characters

## Goal
Restore responsive keystroke delivery to an attached agent tmux pane
even when that pane is producing a heavy stream of output.

## Bead(s)
- bd-c69e23 (P2 task) — TUI: can no longer type to an agent while they
  are emitting characters; need bidirectional I/O simultaneously.

## Before state
- `crates/caco-tui/src/tmux.rs` ran an unconditional
  `tmux display-message -p '#{pane_mode}'` probe before every
  `send-keys` call (copy-mode safety added in bd-b56d87).
- The tmux server processes commands serially, so under heavy output
  the probes queued behind the streaming output bytes. Operator
  keystrokes were either dropped or arrived in a burst once the output
  settled, matching the bd-c69e23 report.
- No regression coverage existed for the per-keystroke probe overhead
  or for the copy-mode detection path.

## After state
- A small `(socket, target) -> (Instant, was_copy_mode)` cache
  (TTL 250ms) short-circuits `exit_copy_mode_if_needed` when the most
  recent observation was `normal`. Copy-mode observations are never
  cached as fast-path eligible, so a real entry into copy-mode is
  detected on the next keystroke and the user can never get stuck.
- Probe count during a typing burst drops from 1-per-keystroke to a
  small fraction of that, removing the queueing-behind-output stall.
- Four new tests cover: cold-cache re-probe, normal short-circuit,
  copy-mode never short-circuits, TTL expiry, and per-socket/target
  isolation.

## Diff summary
- `crates/caco-tui/src/tmux.rs`: added `PANE_MODE_CACHE`,
  `PANE_MODE_CACHE_TTL`, `cached_normal_mode`, `record_pane_mode`,
  `_clear_pane_mode_cache_for_tests`. `exit_copy_mode_if_needed` now
  short-circuits on a fresh normal-mode entry and records the
  observation on every successful probe.
- New tests in `tmux::tests`:
  `pane_mode_cache_short_circuits_after_normal_observation`,
  `pane_mode_cache_never_short_circuits_copy_mode`,
  `pane_mode_cache_expires_after_ttl`,
  `pane_mode_cache_keys_are_per_socket_and_target`.

## Operator-takeaway
Typing to an agent while it is emitting heavy output is responsive
again. If a future change extends the TTL or removes the copy-mode
re-probe, watch for the regression where `q` is unexpectedly sent into
a normal pane (would manifest as cursor jumps or missing characters)
or where copy-mode entry is not detected (manifest: arrow keys move
the copy-mode cursor instead of being forwarded to the shell).

## Tests
- `cargo test -p caco-tui --lib pane_mode_cache` -- 4 passed.
- `cargo test-small` -- 4152 passed, 0 failed.
