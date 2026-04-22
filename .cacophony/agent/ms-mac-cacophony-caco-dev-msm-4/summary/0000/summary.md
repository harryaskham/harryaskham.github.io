# bd-13b2a1 — daemon health watchdog self-exit on wedge

## Goal
Eliminate manual SSH-and-pkill recovery for the termux daemon-wedge
pattern (process alive, no port bind, listener thread deadlocked) by
extending the existing bd-3a85f7 health watchdog to self-exit when
the local listener has been unresponsive for too long, so the
supervisor (`caco up`) respawns a fresh daemon that binds cleanly.

## Bead(s)
- bd-13b2a1 (P2 bug) — sgu24 termux daemon wedges into not-listening
  state every few hours. Two reproductions observed by caco-doctor-hel
  in 7h.

## Before state
- The bd-3a85f7 watchdog probed `GET /api/v1/node` every 30s with a
  5s timeout and logged after 3 consecutive failures.
- On a wedged daemon it kept logging indefinitely. Recovery required
  an operator to `ssh <node> -- pkill -f 'caco.*daemon'` and wait for
  the supervisor to respawn.

## After state
- `health_watchdog_loop` now reads
  `CACO_DAEMON_WATCHDOG_SELF_EXIT_THRESHOLD` (default 20). When the
  consecutive-failure counter reaches that value, the watchdog logs a
  `bd-13b2a1` diagnostic, flushes stderr, and calls
  `std::process::exit(87)`.
- `caco up` (the existing supervisor) sees the exit and respawns a
  fresh daemon process which binds the port cleanly — exactly the
  manual recovery, automated.
- Exit code 87 (= bd-13b2a1) is distinctive so post-mortem tooling
  can tell this apart from a panic, SIGKILL, or clean shutdown.
- Setting the env var to `0` disables self-exit and restores the prior
  log-only behaviour for nodes where automatic restart is undesirable.

## Diff summary
- `crates/caco-daemon/src/lib.rs` (`health_watchdog_loop`):
  - Read `CACO_DAEMON_WATCHDOG_SELF_EXIT_THRESHOLD` once at loop start
    (default `20`, i.e. ~10 minutes at 30s probe interval).
  - After the existing log-on-failure block, evaluate the threshold
    and call `std::process::exit(87)` with a flushed-stderr
    diagnostic citing the bead, the listener address, the failure
    count, and the elapsed seconds.

## Operator-takeaway
- Default behaviour: ~10 minutes of total unresponsiveness triggers
  automatic supervisor-respawn. Should make the sgu24-class wedge
  invisible to operators.
- To tune for a wedge-prone node:
  `CACO_DAEMON_WATCHDOG_SELF_EXIT_THRESHOLD=4` (≈ 2 minutes).
- To audit: grep crash log for `bd-13b2a1: health watchdog self-exit`.
- To disable: `CACO_DAEMON_WATCHDOG_SELF_EXIT_THRESHOLD=0`.

## Tests
- `cargo build -p caco-daemon` — clean.
- The exit path itself is a `std::process::exit` and not unit-testable
  in-proc; the threshold parsing is trivial and covered by the
  `unwrap_or(20)` default. Behaviour will be validated in production
  by sgu24's regular wedges (or, ideally, by their absence).
