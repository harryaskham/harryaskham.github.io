# Session summary — bd-5493cd fake-daemon.sh leak guards

## Goal

Stop leaked `fake-daemon.sh` listeners from
`caco-lifecycle-iso-*` test isolation directories from squatting
the real caco daemon's ports (11100 HTTP, 11109 sidecar) across
nix-shell exits and test-runner SIGKILLs.

## Bead(s)

- `bd-5493cd` — fake-daemon.sh from caco-lifecycle-iso tests can leak and conflict with real daemon ports 11100/11109 (P1 bug, port-conflict)

## Before state

bd-63bd11 had already added a trap-on-EXIT/TERM/INT/HUP handler to
the test fake-daemon shell, which runs `kill 0` to terminate the
listener process group on graceful kill. That mitigation only
fires when the shell receives a signal it can trap.

When the test runner itself was SIGKILLed, panicked in a way that
bypassed normal teardown, or was killed by `nix-shell` exit before
its children, the trap never ran. The `nc` listener was reparented
to PID 1 and continued binding the daemon port across runs. Three
such leaks survived 5 days on helsinki, blocking the real daemon
from binding 11100 when it tried to start.

## After state

Two complementary guards added to
`crates/caco-sidecar/src/lifecycle.rs`:

### 1. Parent-PID watchdog inside fake-daemon.sh

The script now spawns a background loop that polls
`kill -0 <test_pid>` on its spawning test runner. When the runner
is gone (any cause, including SIGKILL), the watchdog runs
`kill 0` to terminate the entire listener process group within
~1 s. This closes the SIGKILL hole where the EXIT trap cannot
fire.

### 2. Port-conflict preflight at iso-entry

`isolated_config_and_paths()` now checks whether real daemon ports
11100 / 11109 are bound by a leaked `fake-daemon.sh` before
allocating ephemeral ports. Heuristic: read `/proc/<pid>/cmdline`
and only panic when the cmdline contains `caco-lifecycle-iso-` or
`fake-daemon.sh`. Real daemon ownership (the common dev-machine
case) does not trigger the panic.

The panic message points operators at the leak class and suggests
`pkill -KILL -f caco-lifecycle-iso-` as the immediate-recovery
action — gives a clean test-time signal instead of a silent
production-restart-loop hours later.

## Diff summary

- Commit: c2a9ba008
- Files touched:
  - `crates/caco-sidecar/src/lifecycle.rs` (+122) — preflight
    block at iso-entry, watchdog block in fake-daemon.sh script
    body, new source-grep regression test.
- New `fake_daemon_iso_harness_has_leak_guards_bd_5493cd` greps
  the lifecycle.rs source and asserts:
  - iso-entry preflight covers 11100 and 11109,
  - preflight only panics on the `caco-lifecycle-iso-` /
    `fake-daemon.sh` cmdline shape,
  - fake-daemon.sh script body contains the
    `kill -0 {test_pid}` watchdog loop and `kill 0` group-kill.
- Tests:
  - `cargo test -p caco-sidecar --lib lifecycle`: 118/118 pass.
  - `cargo test-small`: 264/264 pass.

## Operator-takeaway

A SIGKILLed test runner can no longer leak a port-squatting
`nc` listener that survives nix-shell and blocks the real daemon
hours later. Two layers of protection: the listener
self-terminates when its spawning test process dies, and any
leak that does slip through is caught at the *next* iso test's
entry preflight with a clear pointer to the recovery command.
