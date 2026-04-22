# Session summary — bd-30a15c: stop sidecar EADDRINUSE recovery from self-killing the test process

## Goal

Unbreak the broken-on-main `serve_eaddrinuse_reports_clear_error`
test in `caco-sidecar` and remove a latent foot-gun from the
EADDRINUSE recovery path that could SIGTERM the very process trying
to recover from a bind collision.

## Bead(s)

- `bd-30a15c` — [broken-on-main] caco-sidecar lib test serve_eaddrinuse_reports_clear_error hangs (SIGTERM) (P2, bug)
- (related: `bd-4db921` introduced the EADDRINUSE recovery path)

## Before state

- Failing test on `main`: `cargo test -p caco-sidecar --lib
  serve_eaddrinuse_reports_clear_error` reproducibly killed by
  SIGTERM (signal 15) on hosts where lsof reports in-process tokio
  listeners.
- Root cause: serve()'s recovery path identifies the port holder via
  lsof. When the holder is our own PID (the test pre-binds the port),
  resolve_pid_exe returns the test binary path
  (`caco_sidecar-<hash>`), which begins with "caco", so kill_pid sends
  SIGTERM to the test process itself.
- The full `caco-sidecar` lib suite ran with three other unrelated
  pre-existing failures (`resolve_pid_exe_returns_self`,
  `stale_detection_true_when_binary_path_mismatches`,
  `status_reports_stopped_when_nothing_running`) — those are not in
  scope of this bead.

## After state

- `serve_eaddrinuse_reports_clear_error` passes individually and in
  the full lib run. Wrapped in a 15s tokio::time::timeout so any
  future regression fails as a clear timeout rather than a SIGTERM.
- `kill_pid_refuses_to_kill_current_process` (new) explicitly pins
  the self-kill guard.
- `cargo test-small` — 4137 passed, 0 failed.
- Pre-existing unrelated failures listed above are still on main.

## Diff summary

- Commits: 07abe4c8
- Files touched:
  - `crates/caco-sidecar/src/lib.rs` — serve() returns a clear
    `Bind` error when find_port_holder returns our own PID; test
    wrapped in tokio::time::timeout
  - `crates/caco-sidecar/src/lifecycle.rs` — kill_pid refuses to act
    on the current PID; new unit test pins the contract
- Tests: +1 / -0 / hardened 1
- Behavioural delta: serve() can no longer SIGTERM the calling
  process via its EADDRINUSE recovery path. The error returned in
  that case is a `Bind` error referencing bd-30a15c so future
  triage is one grep away.

## Operator-takeaway

This is a small but nasty class of bug — a recovery path that kills
the very process running it. The two guards (one in serve(), one in
kill_pid) are independent layers of defence. If you ever see a
sidecar test fail with `signal: 15, SIGTERM` again, search for
`bd-30a15c` in the error output: that string in a Bind error is
proof the new guard fired and the process the recovery wanted to
kill was itself. Anything else points to a different self-kill route
that the kill_pid guard would have caught — and you should add a
third layer at the new call site rather than removing the existing
guards.
